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
        _on_message => $args{on_message} || sub {},
    };
    bless $self, $class;

    if (defined $self->{_config}->get('logger')) {
        $self->{_syslog} = 1
            if $self->{_config}->get('logger') eq 'syslog';
        $self->{_stderr} = 1
            if $self->{_config}->get('logger') eq 'stderr';
    }

    foreach my $level (qw/err warn info debug/) {
        no strict 'refs';
        *{"$class::$level"} = sub {
            my $self = shift;
            my $format = shift;
            my $message = @_ > 0 ? sprintf($format, @_) : $format;
            return $self->log_message($message, $level);
        } if not defined *{"$class::$level"}{CODE};
    }

    return $self;
}

sub on_message {
    my ($self, $callback) = @_;
    $self->{_on_message} = $callback;
}

sub log_message {
    my ($self, $message, $priority) = @_;
    $priority ||= 'warning';

    if ($self->{_syslog}) {
        openlog('emux', 'cons,pid', 'user');
        syslog($priority, '%s', $message);
        closelog();
    }
    elsif ($self->{_stderr}) {
        my $timestamp = strftime "%F %T", localtime;
        print STDERR "[$timestamp $$]: $message\n";
    }

    $self->{_on_message}->($message)
        if $self->{_on_message};

    return $message;
}

sub force_syslog {
    shift->{_syslog} = 1;
}

1;
