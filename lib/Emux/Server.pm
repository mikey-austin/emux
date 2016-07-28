package Emux::Server;

use strict;
use warnings;

use Emux::Message;
use Emux::CommandFactory;
use IO::Select;
use IO::Handle;
use IO::Socket::INET;
use IO::Socket::UNIX;
use List::Util qw(any);
use POSIX ();

sub new {
    my ($class, $config, $logger) = @_;
    my $self = {
        _pid       => undef,
        _config    => $config,
        _logger    => $logger,
        _listeners => []
    };
    bless $self, $class;

    $self->{_cmd_factory} = Emux::CommandFactory->new($self);

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

    my $select = IO::Select->new();
    $self->register_listeners($select);

    for (;;) {
        while (my @ready = $select->can_read) {
            foreach my $handle (@ready) {
                if ($self->is_listener($handle)) {
                    my $new = $handle->accept;
                    $select->add($new);
                }
                else {
                    my ($response, $message);
                    if (($message = Emux::Message->from_handle($handle, $self->{_cmd_factory}))
                        and defined $message
                        and defined $message->command)
                    {
                        $response = $message->command->execute;
                    }
                    else {
                        $self->{_logger}->err('could not understand message');
                        $response = Emux::Message->new(
                            Emux::Message->TYPE_ERROR, $self->{_cmd_factory});
                        $response->body('invalid message');
                    }

                    print $handle $response->encode;
                    $select->remove($handle);
                    $handle->close;
                }
            }
        }
    }

    # Never reached.
}

sub register_listeners {
    my ($self, $select) = @_;

    unless ($self->{_config}->get('daemonize')) {
        $select->add(*STDIN);
        $self->{_logger}->info("Listening on STDIN");
    }

    if (my $socket = $self->{_config}->get('socket')) {
        my $fh = IO::Socket::UNIX->new(
            Local  => $socket,
            Type   => SOCK_STREAM,
            Listen => 5,
        ) or die "Could not initialize socket at $socket: $!";
        $select->add($fh);
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
        $select->add($fh);
        push @{$self->{_listeners}}, $fh;
        $self->{_logger}->info("Listening on $host:$port");
    }
}

sub is_listener {
    my ($self, $fh) = @_;
    any { $_ == $fh } @{$self->{_listeners}};
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
