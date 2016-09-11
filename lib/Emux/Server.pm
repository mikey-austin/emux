package Emux::Server;

use strict;
use warnings;

use Emux::Message qw(:constants);
use Emux::Client;
use Emux::CommandFactory;
use Emux::ProcessManager;
use IO::Poll;
use IO::Handle;
use IO::Socket::INET;
use IO::Socket::UNIX;
use MIME::Base64 qw(encode_base64);
use List::Util qw(any);
use POSIX ();

sub new {
    my ($class, $config, $logger) = @_;
    my $self = {
        _pid         => undef,
        _config      => $config,
        _logger      => $logger,
        _listeners   => [],
        _procs       => {},
        _proc_errors => {},
        _muted       => {},
        _clients     => {
            $config->get('broadcast_stdout')
                ? ( "*STDOUT" => Emux::Client->new(handle => *STDOUT) ) : ()
        },
        _poll        => IO::Poll->new,
    };
    bless $self, $class;

    $self->{_cmd_factory} = Emux::CommandFactory->new($self);
    $self->{_proc_manager} = Emux::ProcessManager->new(
        config => $config,
        logger => $logger,
    );

    return $self;
}

sub start {
    my $self = shift;

    # If socket already exists, exit.
    if (defined $self->{_config}->get('socket')) {
        die 'Socket exists, exiting...'
            if -e $self->{_config}->get('socket');
    }

    die 'PID file exists, exiting...'
        if -e $self->{_config}->get('pidfile');

    $self->daemonize
        if $self->{_config}->get('daemonize');
    $self->save_pid;
    $self->register_signals;
    $self->register_listeners;

    # Set a callback to send logging messages in packets.
    $self->{_logger}->on_message(
        sub {
            my $log_message = shift;
            my $message = Emux::Message->new(TYPE_ERROR_OUTPUT);
            $message->body({
                content => encode_base64($log_message),
            });
            $self->broadcast_message($message);
        }
    );

    for (;;) {
        # Readers.
        while ($self->{_poll}->poll) {
            foreach my $handle ($self->{_poll}->handles(POLLIN)) {
                if ($self->is_listener($handle)) {
                    my $client = $self->accept_client($handle);
                    $self->{_logger}->info(
                        'Accepted connection from %s', $client->from);
                    $self->{_poll}->mask($client->fh, POLLIN | POLLOUT);
                    $self->{_clients}->{$client->fh} = $client;
                }
                elsif ($self->is_proc_fh($handle)) {
                    my $process = $self->{_procs}->{$handle};
                    $self->handle_proc_output(
                        TYPE_OUTPUT, $process, $handle);
                }
                elsif ($self->is_proc_error_fh($handle)) {
                    my $process = $self->{_proc_errors}->{$handle};
                    $self->handle_proc_output(
                        TYPE_ERROR_OUTPUT, $process, $handle);
                }
                elsif (my $client = $self->{_clients}->{$handle}) {
                    my ($message, $error);
                    eval {
                        if ($client->connected) {
                            $message = $client->receive($self->{_cmd_factory});
                            $message->command->execute
                                if $message and $message->command;
                        }
                        else {
                            $self->disconnect($handle);
                        }
                        1;
                    } or do {
                        $error = $@;
                    };

                    if ($error) {
                        $self->{_logger}->err("error processing message: $error");
                    }
                    elsif (not ($message and $message->command) and not $client->connected) {
                        $self->disconnect($handle);
                    }
                }
            }

            foreach my $handle ($self->{_poll}->handles(POLLOUT)) {
                if ($self->is_proc_fh($handle)) {
                    my $process = $self->{_procs}->{$handle};
                    $self->handle_proc_input($process, $handle);
                }
            }
        }
    }

    # Never reached.
}

sub accept_client {
    my ($self, $handle) = @_;

    my $class = 'Emux::Client';
    if ($self->{_config}->get('websocket')) {
        require Emux::Client::Websocket;
        $class = 'Emux::Client::Websocket'
            if UNIVERSAL::isa($handle, 'IO::Socket::INET');
    }

    my $client = $class->new(
        handle        => scalar $handle->accept,
        ws_upgradable => ($self->{_config}->get('websocket') // 0),
    );

    return $client;
}

sub disconnect {
    my ($self, $handle) = @_;
    $self->{_poll}->remove($handle);
    $handle->close
        if $handle->can('close');
    delete $self->{_clients}->{$handle};
}

sub register_listeners {
    my $self = shift;

    my $listeners = 0;
    if ($self->{_config}->get('listen_on_stdin')
        and not $self->{_config}->get('daemonize'))
    {
        $self->{_poll}->mask(*STDIN, POLLIN);
        $self->{_logger}->info("Listening on STDIN");
        $listeners++;
    }

    if (my $socket = $self->{_config}->get('socket')) {
        my $fh = IO::Socket::UNIX->new(
            Local  => $socket,
            Type   => SOCK_STREAM,
            Listen => 5,
        ) or die "Could not initialize socket at $socket: $!";
        $self->{_poll}->mask($fh, POLLIN);
        push @{$self->{_listeners}}, $fh;
        $self->{_logger}->info("Listening on $socket");
        $listeners++;
    }

    my ($host, $port) = $self->{_config}->get('host', 'port');
    if ($host and $port) {
        my $fh = IO::Socket::INET->new(
            LocalAddr => $host,
            LocalPort => $port,
            Proto     => 'tcp',
            ReuseAddr => 1,
            Listen    => 5,
            Type      => SOCK_STREAM,
        ) or die "Could not bind at address $host:$port: $!\n";
        $self->{_poll}->mask($fh, POLLIN);
        push @{$self->{_listeners}}, $fh;
        $self->{_logger}->info("Listening on $host:$port");
        $listeners++;
    }
    elsif ($self->{_config}->get('websocket')) {
        die 'host & port options must be specified if using websockets';
    }

    die 'no listeners defined'
        if $listeners == 0;
}

sub is_listener {
    my ($self, $fh) = @_;
    any { $fh and $_ == $fh } @{$self->{_listeners}};
}

sub proc_manager {
    shift->{_proc_manager};
}

sub mute_ids {
    my ($self, @ids) = @_;
    foreach my $id (@ids) {
        $self->{_muted}->{$id} = $id;
    }
}

sub unmute_ids {
    my ($self, @ids) = @_;
    foreach my $id (@ids) {
        delete $self->{_muted}->{$id};
    }
}

sub is_muted {
    my ($self, $proc) = @_;
    return exists $self->{_muted}->{$proc->id};
}

sub schedule_write {
    my ($self, $input, @ids) = @_;
    foreach my $id (@ids) {
        push @{$self->{_writers}->{$id}}, $input;
    }
}

sub register_process {
    my ($self, $process) = @_;
    eval {
        $self->{_proc_manager}->run_process($process);
        $self->{_poll}->mask($process->fh, POLLIN | POLLOUT);
        $self->{_poll}->mask($process->errors, POLLIN);
        $self->{_procs}->{$process->fh} = $process;
        $self->{_procs}->{$process->id} = $process;
        $self->{_proc_errors}->{$process->errors} = $process;
        1;
    } or do {
        my $error = $@;
        $self->{_logger}->err(
            'could not start process %s; %s', $process->id, $error);
    };
}

sub deregister_process {
    my ($self, $process, $exit_status) = @_;
    $self->{_poll}->remove($process->fh);
    $self->{_poll}->remove($process->errors);
    delete $self->{_procs}->{$process->fh};
    delete $self->{_procs}->{$process->id};
    delete $self->{_proc_errors}->{$process->errors};
    delete $self->{_muted}->{$process->id};
    delete $self->{_writers}->{$process->id};

    # Broadcast a finished message.
    my $message = Emux::Message->new(TYPE_FINISHED);
    $message->body({
        id        => $process->id,
        tags      => $process->tags,
        exit_code => int($exit_status) || 0,
    });
    $self->broadcast_message($message);
}

sub is_proc_fh {
    my ($self, $fh) = @_;
    return $fh && $self->{_procs}->{$fh};
}

sub is_proc_error_fh {
    my ($self, $fh) = @_;
    return $fh && $self->{_proc_errors}->{$fh};
}

sub handle_proc_input {
    my ($self, $process, $handle) = @_;

    if (exists $self->{_writers}->{$process->id}
        and @{$self->{_writers}->{$process->id}} > 0)
    {
        $self->{_logger}->debug('writing to ' . $process->id);
        my $input = shift @{$self->{_writers}->{$process->id}};
        print { $handle } "$input\n";
    }
}

sub handle_proc_output {
    my ($self, $type, $process, $handle) = @_;

    $handle->blocking(0);
    my ($line, $output);
    do {
        $line = $handle->getline;
        $output .= $line
            if $line;
    } while ($line);

    return unless $process and $output;

    unless ($self->{_muted}->{$process->id}) {
        my $message = Emux::Message->new($type);
        $message->body({
            id      => $process->id,
            tags    => $process->tags,
            content => encode_base64($output),
        });
        $self->broadcast_message($message);
    }
    else {
        $self->{_logger}->debug('output for %s muted', $process->id);
    }
}

sub broadcast_message {
    my ($self, $message) = @_;
    $_->send($message)
        foreach values %{$self->{_clients}};
}

sub daemonize {
    my $self = shift;

    # Log to syslog.
    if ($self->{_config}->set('logger') ne 'syslog') {
        $self->{_logger}->force_syslog;
        $self->{_logger}->warn('forced logger to syslog');
    }

    # Drop privileges.
    if (defined $self->{_config}->get('user')) {
        my $uid = getpwnam($self->{_config}->get('user'));
        POSIX::setuid($uid)
            or die "Could not setuid to $uid, exiting...";
    }

    if (defined $self->{_config}->get('group')) {
        my $gid = getgrnam($self->{_config}->get('group'));
        POSIX::setgid($gid)
            or die "Could not setgid to $gid, exiting...";
    }

    # Become session leader.
    POSIX::setsid or die "Could not setsid: $!";

    # Fork a child process.
    my $pid = fork();
    if ($pid < 0) {
        die "Could not fork: $!";
    }
    elsif ($pid) {
        exit;
    }

    # Change root directory and clear file creation mask.
    chdir('/');
    umask(0);

    # Clear all file descriptors.
    POSIX::close($_) foreach (0 .. (POSIX::sysconf(&POSIX::_SC_OPEN_MAX) || 1024));

    open(STDIN, '</dev/null');
    open(STDOUT, '>/dev/null');
    open(STDERR, ">&STDOUT");

    $self->{_pid} = POSIX::getpid;
    $self->{_logger}->info("Daemonized with pid $self->{_pid}");
}

sub save_pid {
    my $self = shift;

    open(PID, '>' . $self->{_config}->get('pidfile'))
        or die 'Could not open pidfile for writing';
    print PID $$;
    close PID;
}

sub register_signals {
    my $self = shift;

    $SIG{'PIPE'} = 'IGNORE';
    $SIG{'INT'} = $SIG{'TERM'} = sub {
        $self->shutdown;
        exit;
    };
}

sub shutdown {
    my ($self, $error) = @_;

    if (defined $error) {
        $self->{_logger}->err("Error encountered: $error");
    }
    else {
        $self->{_logger}->info("Shutting down emux $$...");
    }

    unlink($self->{_config}->get('socket'))
        if $self->{_config}->get('socket') and -e $self->{_config}->get('socket');
    unlink($self->{_config}->get('pidfile'))
        if -e $self->{_config}->get('pidfile');
}

1;
