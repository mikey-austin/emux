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

sub execute {
    # Noop.
}

sub output {
    my $self = shift;
    return {
        type => $self->{_type}
    };
}

1;
