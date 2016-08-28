package Emux::Command::Unmute;

use strict;
use warnings;
use parent qw(Emux::Command::Mute);

sub process_ids {
    my ($self, @ids) = @_;
    $self->server->unmute_ids(@ids);
}

1;
