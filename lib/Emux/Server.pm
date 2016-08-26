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
        _pid       => undef,
        _config    => $config,
        _logger    => $logger,
        _listeners => [],
        _procs     => {},
        _clients   => [ *STDOUT ],
        _select    => IO::Select->new,
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
                #content => base64_encode($log_message),
                content => $log_message,
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
                    push @{$self->{_clients}}, $client;
                }
                elsif ($self->is_proc_fh($handle)) {
                    # Read output and broadcast output message in chunks.
                    my ($line, $output);
                    my $process = $self->{_procs}->{fileno($handle)};
                    my $select = IO::Select->new($handle);
                    do {
                        $line = readline($handle);
                        $output .= $line
                            if $line;
                    } while($line and $select->can_read(1));
                    next if not $output;

                    my $message = Emux::Message->new(TYPE_OUTPUT);
                    $message->body({
                        id      => $process->id,
                        #content => base64_encode($output),
                        content => $output,
                    });
                    $self->broadcast_message($message);
                }
                else {
                    my ($message, $error);
                    eval {
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
                        $self->{_logger}->err('could not understand message');
                    }

                    # Keep connection open for the moment...
                    #$self->{_select}->remove($handle);
                    #$handle->close;
                }
            }
        }
    }

    # Never reached.
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
    any { fileno($_) == fileno($fh) } @{$self->{_listeners}};
}

sub proc_manager {
    shift->{_proc_manager};
}

sub register_process {
    my ($self, $process) = @_;
    $self->{_proc_manager}->run_process($process);
    $self->{_select}->add($process->fh);
    $self->{_procs}->{fileno($process->fh)} = $process;
}

sub deregister_process {
    my ($self, $process, $exit_status) = @_;
    $self->{_select}->remove($process->fh);
    delete $self->{_procs}->{fileno($process->fh)};

    # Broadcast a finished message.
    my $message = Emux::Message->new(TYPE_FINISHED);
    $message->body({
        id        => $process->id,
        exit_code => $exit_status || 0,
    });
    $self->broadcast_message($message);
}

sub is_proc_fh {
    my ($self, $fh) = @_;
    return defined $self->{_procs}->{fileno($fh)};
}

sub broadcast_message {
    my ($self, $message) = @_;
    printf { $_ } "%s\n", $message->encode
        foreach @{$self->{_clients}};
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
