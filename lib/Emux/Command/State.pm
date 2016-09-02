package Emux::Command::State;

use strict;
use warnings;
use parent qw(Emux::Command);

use Emux::Message qw(:constants);

sub execute {
    my $self = shift;

    my $server = $self->server;
    my $proc_manager = $server->proc_manager;
    my $message = Emux::Message->new(TYPE_STATE);
    $message->body({
        tags  => [ $proc_manager->running_tags ],
        processes => [
            map {
                id      => $_->id,
                machine => $_->host,
                muted   => $server->is_muted($_)? \1 : \0,
                command => $_->command,
                created => int $_->created,
                tags    => $_->tags,
            }, $proc_manager->procs
        ]
    });
    $server->broadcast_message($message);
}

1;
