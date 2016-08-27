package Emux::Server;

use strict;
use warnings;

use Emux::Message qw(:constants);
use Emux::CommandFactory;
use Emux::ProcessManager;
use IO::Select;
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
        _clients     => {},
        _select      => IO::Select->new,
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
        while (my @ready = $self->{_select}->can_read) {
            foreach my $handle (@ready) {
                if ($self->is_listener($handle)) {
                    my $client = $handle->accept;
                    my $from;
                    if ($client->can('peerhost')) {
                        my ($peer, $port) = ($client->peerhost, $client->peerport);
                        $from = "$peer:$port";
                    }
                    elsif ($client->can('hostpath')) {
                        $from = $client->hostpath;
                    }
                    $self->{_logger}->info("Accepted connection from $from");
                    $self->{_select}->add($client);
                    $self->{_clients}->{$client} = $client;
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
                else {
                    my ($message, $error);
                    eval {
                        # Check if the client disconnected.
                        $self->disconnect($handle)
                            unless $handle->can('connected') and $handle->connected;

                        $message = Emux::Message->from_handle($handle, $self->{_cmd_factory});
                        $message->command->execute
                            if $message and $message->command;
                        1;
                    } or do {
                        $error = $@;
                    };

                    if ($error) {
                        $self->{_logger}->err("error processing message: $error");
                    }
                    elsif (not (defined $message and defined $message->command)) {
                        $self->disconnect($handle);
                    }
                }
            }
        }
    }

    # Never reached.
}

sub disconnect {
    my ($self, $handle) = @_;

    # Make sure it is a client.
    if (defined $self->{_clients}->{$handle}) {
        $self->{_logger}->info('client disconnected');
        $self->{_select}->remove($handle);
        $handle->close
            if $handle->can('close');
        delete $self->{_clients}->{$handle};
    }
}

sub register_listeners {
    my $self = shift;

    unless ($self->{_config}->get('daemonize')) {
        $self->{_select}->add(*STDIN);
        $self->{_logger}->info("Listening on STDIN");
    }

    if (my $socket = $self->{_config}->get('socket')) {
        my $fh = IO::Socket::UNIX->new(
            Local  => $socket,
            Type   => SOCK_STREAM,
            Listen => 5,
        ) or die "Could not initialize socket at $socket: $!";
        $self->{_select}->add($fh);
        push @{$self->{_listeners}}, $fh;
        $self->{_logger}->info("Listening on $socket");
    }

    my ($host, $port);
    if (($host, $port) = $self->{_config}->get('host', 'port')
        and defined $host and defined $port)
    {
        my $fh = IO::Socket::INET->new(
            LocalAddr => $host,
            LocalPort => $port,
            Proto     => 'tcp',
            ReuseAddr => 1,
            Listen    => 5,
            Type      => SOCK_STREAM,
        ) or die "Could not bind at address $host:$port: $!\n";
        $self->{_select}->add($fh);
        push @{$self->{_listeners}}, $fh;
        $self->{_logger}->info("Listening on $host:$port");
    }
}

sub is_listener {
    my ($self, $fh) = @_;
    any { $_ == $fh } @{$self->{_listeners}};
}

sub proc_manager {
    shift->{_proc_manager};
}

sub register_process {
    my ($self, $process) = @_;
    $self->{_proc_manager}->run_process($process);
    $self->{_select}->add($process->fh);
    $self->{_select}->add($process->errors);
    $self->{_procs}->{$process->fh} = $process;
    $self->{_proc_errors}->{$process->errors} = $process;
}

sub deregister_process {
    my ($self, $process, $exit_status) = @_;
    $self->{_select}->remove($process->fh);
    delete $self->{_procs}->{$process->fh};
    delete $self->{_proc_errors}->{$process->errors};

    # Broadcast a finished message.
    my $message = Emux::Message->new(TYPE_FINISHED);
    $message->body({
        id        => $process->id,
        exit_code => int($exit_status) || 0,
    });
    $self->broadcast_message($message);
}

sub is_proc_fh {
    my ($self, $fh) = @_;
    return defined $self->{_procs}->{$fh};
}

sub is_proc_error_fh {
    my ($self, $fh) = @_;
    return defined $self->{_proc_errors}->{$fh};
}

sub handle_proc_output {
    my ($self, $type, $process, $handle) = @_;

    # Read output and broadcast output message in chunks.
    my ($line, $output);
    my $select = IO::Select->new($handle);
    do {
        $line = readline($handle);
        $output .= $line
        if $line;
    } while ($line and $select->can_read(1));
    return if not $output;

    my $message = Emux::Message->new($type);
    $message->body({
        id      => $process->id,
        content => encode_base64($output),
    });
    $self->broadcast_message($message);
}

sub broadcast_message {
    my ($self, $message) = @_;
    printf { $_ } "%s\n", $message->encode
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
