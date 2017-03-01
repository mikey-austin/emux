package Emux::Logger;

use strict;
use warnings;

use POSIX qw(strftime);
use Sys::Syslog qw(:standard :macros);

sub new {
    my ($class, $config, %args) = @_;
    my $self = {
        _config     => $config,
        _syslog     => 0,
        _stderr     => 0,
        _prefix     => 1,
        _on_message => $args{on_message} || sub {},
    };
    bless $self, $class;

    $self->logger($self->{_config}->get('logger'))
        if defined $self->{_config}->get('logger');

    foreach my $level (qw/err warning info debug/) {
        no strict 'refs';
        *{"${class}::$level"} = sub {
            my $self = shift;
            my $format = shift;
            my $message = @_ > 0 ? sprintf($format, @_) : $format;
            return $self->log_message($message, $level);
        } if not defined *{"${class}::$level"}{CODE};
    }

    return $self;
}

sub on_message {
    my ($self, $callback) = @_;
    $self->{_on_message} = $callback;
}

sub logger {
    my ($self, $logger) = @_;
    $self->{_syslog} = 1
        if $logger eq 'syslog';
    $self->{_stderr} = 1
        if $logger eq 'stderr';
}

sub prefix {
    my ($self, $prefix) = @_;
    $self->{_prefix} = $prefix ? 1 : 0;
}

sub warn {
    my $self = shift;
    $self->warning(@_);
}

sub log_message {
    my ($self, $message, $priority) = @_;
    $priority ||= 'warning';

    if ($self->{_syslog}) {
        eval {
            openlog('emux', 'cons,pid', 'user');
            syslog($priority, '%s', $message);
            closelog();
            1;
        } or do {
            my $error = $@;
            CORE::warn "Could not write to syslog: $error";
        };
    }
    elsif ($self->{_stderr}) {
        my $timestamp = strftime "%F %T", localtime;
        my $prefix = $self->{_prefix} ? "[$timestamp $$]: " : '';
        print STDERR "$prefix$message\n";
    }

    $self->{_on_message}->($message)
        if $self->{_on_message};

    return $message;
}

sub force_syslog {
    shift->{_syslog} = 1;
}

1;
