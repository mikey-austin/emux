package Emux::Command::Execute;

use strict;
use warnings;
use parent qw(Emux::Command);

sub execute {
    my $self = shift;
    require Data::Dumper;
    print "running command " . Data::Dumper::Dumper($self->{_message}) . "\n";
}

sub output {
    my $self = shift;

    my $output = $self->SUPER::output;
    $output->{message} = $self->{_message}->body;

    return $output;
}

1;
