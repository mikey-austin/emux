package Emux::Command;

use strict;
use warnings;

sub new {
    my ($class, $type, $message) = @_;
    bless {
        _type    => $type,
        _message => $message || undef,
    }, $class;
}

sub message {
    my ($self, $message) = @_;
    $self->{_message} = $message if defined $message;
    return $self->{_message};
}

sub execute {
    # Noop.
}

sub output {
    return {
        type => $self->{_type};
    };
}

1;
