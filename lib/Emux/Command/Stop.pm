package Emux::Command::Stop;

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

    my $proc_manager = $self->server->proc_manager;
    $proc_manager->stop_process_ids(@ids)
        if @ids > 0;
}

1;
