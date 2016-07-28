package Emux::Command::Error;

use strict;
use warnings;
use parent qw(Emux::Command);

sub output {
    my $self = shift;

    my $output = $self->SUPER::output;
    $output->{message} = $self->{_message}->body;

    return $output;
}

1;
