package Emux::Command;

use strict;
use warnings;

sub new {
    my ($class, $type, $server) = @_;
    bless {
        _type    => $type,
        _message => undef,
        _server  => $server,
    }, $class;
}

sub message {
    my ($self, $message) = @_;
    $self->{_message} = $message if defined $message;
    return $self->{_message};
}

sub server {
    my ($self, $server) = @_;
    $self->{_server} = $server if defined $server;
    return $self->{_server};
}

sub proc_manager {
    shift->{_server}->proc_manager;
}

sub execute {
    ...
}

1;
