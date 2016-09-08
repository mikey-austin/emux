package Emux::Client;

use strict;
use warnings;

use Emux::Message;

sub new {
    my ($class, %args) = @_;
    my $self = {
        _handle        => $args{handle},
        _ws_upgradable => $args{ws_upgradable},
    };

    bless $self, $class;
}

sub fh {
    shift->{_handle};
}

sub from {
    my $self = shift;

    my $from = '';
    if ($self->{_handle}->can('peerhost')) {
        my ($peer, $port) =
            ($self->{_handle}->peerhost, $self->{_handle}->peerport);
        $from = "$peer:$port";
    }
    elsif ($self->{_handle}->can('hostpath')) {
        $from = $self->{_handle}->hostpath;
    }

    return $from;
}

sub send {
    my ($self, $message) = @_;
    printf { $self->fh } "%s\n", $message->encode;
}

sub receive {
    my ($self, $factory) = @_;
    Emux::Message->from_handle($self->fh, $factory);
}

sub connected {
    my $self = shift;

    if ($self->fh and $self->fh->can('connected')) {
        return $self->fh->connected;
    }

    return 1;
}

1;
