package Emux::Command::Input;

use strict;
use warnings;
use parent qw(Emux::Command);

sub execute {
    my $self = shift;

    # Expand arguments into list of process ids.
    my $message = $self->message;
    my @ids = $self->expand_and_merge_tags(
        $message->{_body}->{id} || [],
        $message->{_body}->{tags} || [],
    );

    my $input = $message->{_body}->{input}
        or die 'input is required';
    $self->server->schedule_write($input, @ids)
        if @ids > 0;
}

1;
