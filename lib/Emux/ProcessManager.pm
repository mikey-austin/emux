package Emux::ProcessManager;

use strict;
use warnings;

use POSIX qw(:sys_wait_h :unistd_h);
use Socket;
use IO::Handle;
use Emux::Process;

sub new {
    my ($class, %args) = @_;
    my $self = {
        _config      => $args{config},
        _logger      => $args{logger},
        _master_opts => $args{master_opts} || '',
        _procs       => {},
        _masters     => {},
        _tags        => {},
    };
    bless $self, $class;

    $self->_register_signals;

    return $self;
}

sub run_process {
    my ($self, $process) = @_;

    die sprintf "process %s already exists\n", $process->id
        if $self->{_procs}->{id}->{$process->id};

    $self->_check_master($process);
    my ($pid, $fh, $errors) = $self->_fork(
        routine => sub {
            my ($fh, $errors) = @_;
            # Setup stdout & stdin to be over the supplied filehandle.
            if (defined $fh) {
                dup2(fileno($fh), STDIN_FILENO);
                dup2(fileno($fh), STDOUT_FILENO);
                dup2(fileno($errors), STDERR_FILENO);
            }
            $self->{_logger}->debug('running process id %s', $process->id);
            $process->run;
        }
    );
    $process->pid($pid);
    $process->fh($fh);
    $process->errors($errors);
    $self->_register_process($process);
}

sub stop_process_ids {
    my ($self, @ids) = @_;

    foreach my $id (@ids) {
        my $process = $self->{_procs}->{id}->{$id};
        $self->stop_process($process)
            if $process;
    }
}

sub stop_process {
    my ($self, $process) = @_;

    $self->{_logger}->warn(
        'stopping process %s with pid %s',
        $process->id,
        $process->pid
    );
    kill 'TERM', $process->pid;
}

sub procs {
    my $self = shift;
    return map $self->{_procs}->{id}->{$_}, keys %{$self->{_procs}->{id}};
}

sub masters {
    my $self = shift;
    return map $self->{_masters}->{$_}, keys %{$self->{_masters}};
}

sub running_tags {
    keys %{shift->{_tags}}
}

sub expand_tags {
    my ($self, @tags) = @_;

    my %ids;
    foreach my $tag (@tags) {
        if ($self->{_tags}->{$tag}) {
            foreach my $id (keys %{$self->{_tags}->{$tag}}) {
                $ids{$id} = 1;
            }
        }
    }

    return keys %ids;
}

sub _register_process {
    my ($self, $process) = @_;

    $self->{_procs}->{id}->{$process->id} = $process;
    $self->{_procs}->{pid}->{$process->pid} = $process;
    $self->{_masters}->{$process->host}->{procs}->{$process->id} = $process;

    # Register tags.
    foreach my $tag (@{$process->tags}) {
        $self->{_tags}->{$tag} = {} unless $self->{_tags}->{$tag};
        $self->{_tags}->{$tag}->{$process->id} = $process;
    }
}

sub _deregister_process {
    my ($self, $process) = @_;

    $self->{_logger}->debug(
        'de-registering process id %s; %s', $process->id, $process->pid);
    foreach my $tag (@{$process->tags}) {
        delete $self->{_tags}->{$tag}->{$process->id};
        delete $self->{_tags}->{$tag}
            unless %{$self->{_tags}->{$tag}};
    }

    delete $self->{_procs}->{id}->{$process->id};
    delete $self->{_procs}->{pid}->{$process->pid};
    delete $self->{_masters}->{$process->host}->{procs}->{$process->id};
}

sub _check_master {
    my ($self, $process) = @_;

    # Check if master process exists
    $self->_register_master($process->host)
        if not $self->{_masters}->{$process->host};
}

sub _register_master {
    my ($self, $master) = @_;

    # Fork master ssh process.
    my $pid = $self->_fork(
        routine => sub {
            my @options = (
                '-NTM',
                $self->{_master_opts},
            );
            if (my $control_path = $self->{_config}->get('control_path')) {
                push @options, '-S', $control_path;
            }
            push @options, $master;
            $self->{_logger}->debug(
                "starting master; %s %s",
                'ssh', join (' ', @options)
            );
            exec join ' ', ('/usr/bin/ssh', @options);
        }
    );

    $self->{_masters}->{$master} = {
        host  => $master,
        pid   => $pid,
        procs => {}
    };
    $self->{_masters_by_pid}->{$pid} = $self->{_masters}->{$master};
}

sub _deregister_master {
    my ($self, $master) = @_;

    $self->{_logger}->debug('de-registering master %s', $master);
    delete $self->{_masters_by_pid}->{$self->{_masters}->{$master}->{pid}};
    delete $self->{_masters}->{$master};
}

sub _stop_master {
    my ($self, $master) = @_;

    if (defined $self->{_masters}->{$master}->{pid}) {
        kill 'TERM', $self->{_masters}->{$master}->{pid};
        $self->{_logger}->warn(
            'sent SIGTERM to master %s with pid %s',
            $master,
            $self->{_masters}->{$master}->{pid}
        );
    }
}

sub _fork {
    my ($self, %args) = @_;

    die 'routine required'
        if not exists $args{routine};

    my ($child_handle, $parent_handle, $child_errors, $parent_errors);
    my $setup_fh = wantarray ? 1 : 0;
    if ($setup_fh) {
        # Setup pipes to forked process if called in array context.
        socketpair($child_handle, $parent_handle, AF_UNIX, SOCK_STREAM, PF_UNSPEC)
            or die "socketpair failed: $!";
        socketpair($child_errors, $parent_errors, AF_UNIX, SOCK_STREAM, PF_UNSPEC)
            or die "socketpair failed: $!";
        $child_handle->autoflush(1);
        $parent_handle->autoflush(1);
        $child_errors->autoflush(1);
        $parent_errors->autoflush(1);
    }

    my $pid;
    if(($pid = fork()) == 0) {
        $self->{_logger}->on_message(undef);
        $self->{_logger}->prefix(0);

        # In child.
        if ($setup_fh) {
            close($parent_handle);
            close($parent_errors);
        }
        $SIG{'CHLD'} = 'IGNORE';
        $SIG{'PIPE'} = 'IGNORE';
        $args{routine}->($child_handle, $child_errors);
        exit(0);
    }
    elsif(not defined $pid) {
        $self->{_logger}->err("could not fork: $!");
    }
    else {
        # In parent.
        if ($setup_fh) {
            close($child_handle);
            close($child_errors);
        }
    }

    return $setup_fh ? ($pid, $parent_handle, $parent_errors) : $pid;
}

sub _register_signals {
    my $self = shift;

    $SIG{'CHLD'} = sub {
        my $pid;
        for ($pid = waitpid(-1, WNOHANG); $pid > 0; $pid = waitpid(-1, WNOHANG)) {
            my $exit_status = WEXITSTATUS($?);
            $self->{_logger}->debug("in SIGCHLD; $pid exited with $exit_status");

            if ($self->{_procs}->{pid}->{$pid}) {
                my $process = $self->{_procs}->{pid}->{$pid};
                eval {
                    $process->on_exit($exit_status);
                    $self->_deregister_process($process);
                    1;
                } or do {
                    my $error = $@;
                    $self->{_logger}->err(
                        'error in %s on_exit handler; %s', $process->id, $error);
                };
            }
            elsif ($self->{_masters_by_pid}->{$pid}) {
                my $master = $self->{_masters_by_pid}->{$pid};
                $self->{_logger}->warn('master %s exited with %i', $master, $exit_status);
                eval {
                    $self->_deregister_master($master->{host});
                } or do {
                    my $error = $@;
                    $self->{_logger}->err('error deregistering master; %s', $error);
                };
            }
            else {
                $self->{_logger}->warn("unknown process $pid exited with $exit_status");
            }

        }
    };
}

1;
