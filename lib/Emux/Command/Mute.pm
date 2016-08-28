package Emux::Command::Mute;

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

    $self->process_ids(@ids)
        if @ids > 0;
}

sub process_ids {
    my ($self, @ids) = @_;
    $self->server->mute_ids(@ids);
}

1;
