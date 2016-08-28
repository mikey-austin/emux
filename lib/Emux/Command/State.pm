package Emux::Command::State;

use strict;
use warnings;
use parent qw(Emux::Command);

use Emux::Message qw(:constants);

sub execute {
    my $self = shift;

    my $proc_manager = $self->server->proc_manager;
    my $message = Emux::Message->new(TYPE_STATE);
    $message->body({
        tags => [ $proc_manager->running_tags ],
        processes => [
            map {
                id      => $_->id,
                host    => $_->host,
                command => $_->command,
                created => int $_->created,
                tags    => $_->tags,
            }, $proc_manager->procs
        ]
    });
    $self->server->broadcast_message($message);
}

1;
