package Emux::Logger;

use strict;
use warnings;

use POSIX qw(strftime);
use Sys::Syslog qw(:standard :macros);

sub new {
    my ($class, $config) = @_;
    my $self = {
        _config => $config,
        _syslog => 0,
    };
    bless $self, $class;

    if (defined $self->{_config}->get('logger')
        and $self->{_config}->get('logger') eq 'syslog')
    {
        $self->{_syslog} = 1;
    }

    return $self;
}

sub err {
    my ($self, $message) = @_;
    return $self->log_message($message, 'err');
}

sub debug {
    my ($self, $message) = @_;
    return $self->log_message($message, 'debug');
}

sub info {
    my ($self, $message) = @_;
    return $self->log_message($message, 'info');
}

sub log_message {
    my ($self, $message, $priority) = @_;
    $priority ||= 'warning';

    if ($self->{_syslog}) {
        openlog('emux', 'cons,pid', 'user');
        syslog($priority, '%s', $message);
        closelog();
    }
    else {
        my $timestamp = strftime "%F %T", localtime;
        print STDERR "[$timestamp]: $message\n";
    }

    return $message;
}

sub force_syslog {
    shift->{_syslog} = 1;
}

1;
