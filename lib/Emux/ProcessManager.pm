package Emux::ProcessManager;

use strict;
use warnings;

use POSIX;
use Socket;
use IO::Handle;
use Emux::Process;

sub new {
    my ($class, %args) = @_;
    my $self = {
        _config      => $args{config},
        _logger      => $args{logger},
        _io          => $args{io},
        _master_opts => $args{master_opts} || '',
        _procs       => {},
        _masters     => {},
    };
    bless $self, $class;

    $self->_register_signals;

    return $self;
}

sub run_process {
    my ($self, $process) = @_;

    die sprintf 'process %s already exists', $process->id
        if defined $self->{_procs}->{$process->id};

    $self->_check_master($process);
    my ($pid, $fh) = $self->_fork(
        routine => sub {
            my $fh = shift;
            # Setup stdout & stdin.
            if (defined $fh) {
                # close(STDOUT);
                # close(STDIN);
                # open(STDOUT, \$fh);
                # open(STDIN, \$fh);
            }
            $self->{_logger}->debug('running process id %s', $process->id);
            $process->run;
        }
    );
    $process->pid($pid);
    $process->fh($fh);
    $self->_register_process($process);
}

sub stop_process {
    my ($self, $process) = @_;
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

sub _register_process {
    my ($self, $process) = @_;

    $self->{_procs}->{id}->{$process->id} = $process;
    $self->{_procs}->{pid}->{$process->pid} = $process;
    $self->{_masters}->{$process->host}->{procs}->{$process->id} = $process;
}

sub _deregister_process {
    my ($self, $process) = @_;

    $self->{_logger}->debug(
        'deregistering process id %s; %s', $process->id, $process->pid);
    delete $self->{_procs}->{id}->{$process->id};
    delete $self->{_procs}->{pid}->{$process->pid};
    delete $self->{_masters}->{$process->host}->{procs}->{$process->id};

    # Deregister master if it has no more processes running.
    my @master_procs = keys %{$self->{_masters}->{$process->host}->{procs}};
    $self->_stop_master($process->host)
        if @master_procs == 0;
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
            $self->{_logger}->debug("starting master connection; $master");
            system(
                'ssh', '-TM',          # Master mode with no tty.
                $self->{_master_opts}, # Custom options.
                $master,               # Remote host.
            );
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

    $self->{_logger}->debug('deregistering master %s', $master);
    delete $self->{_masters_by_pid}->{$self->{_masters}->{$master}->{pid}};
    delete $self->{_masters}->{$master};
}

sub _stop_master {
    my ($self, $master) = @_;
    kill 'TERM', $self->{_masters}->{$master}->{pid};
}

sub _fork {
    my ($self, %args) = @_;

    die 'routine required'
        if not exists $args{routine};

    my ($child_handle, $parent_handle);
    my $setup_fh = wantarray ? 1 : 0;
    if ($setup_fh) {
        # Setup pipes to forked process if called in array context.
        socketpair($child_handle, $parent_handle, AF_UNIX, SOCK_STREAM, PF_UNSPEC)
            or die "socketpair failed: $!";
        $child_handle->autoflush(1);
        $parent_handle->autoflush(1);
    }

    my $pid;
    if(($pid = fork()) == 0) {
        # In child.
        if ($setup_fh) {
            close($parent_handle)
        }
        $SIG{'CHLD'} = 'IGNORE';
        $args{routine}->($child_handle);
        $self->{_logger}->warn("fork command finished");
        exit(0);
    }
    elsif(not defined $pid) {
        $self->{_logger}->err("could not fork: $!");
    }
    else {
        # In parent.
        close($child_handle)
            if $setup_fh;
    }

    return $setup_fh ? ($pid, $parent_handle) : $pid;
}

sub _register_signals {
    my $self = shift;

    $SIG{'CHLD'} = sub {
        my $pid;
        do {
            $pid = waitpid(-1, POSIX::WNOHANG);
            my $exit_status = POSIX::WEXITSTATUS($?);
            $self->{_logger}->debug("SIGCHLD received for $pid; exited with $exit_status");

            if ($self->{_procs}->{pid}->{$pid}) {
                my $process = $self->{_procs}->{pid}->{$pid};
                $self->_deregister_process($process);
            }
            elsif ($self->{_masters_by_pid}->{$pid}) {
                my $master = $self->{_masters_by_pid}->{$pid};
                $self->_deregister_master($master->{host});
            }
            else {
                $self->{_logger}->warn("unknown process $pid exited with $exit_status");
            }

        } while($pid > 0);
    };
}

1;
