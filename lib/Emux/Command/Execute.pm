package Emux::Command::Execute;

use strict;
use warnings;
use parent qw(Emux::Command);

use Emux::Process;

sub execute {
    my $self = shift;

    my $message = $self->message;
    $self->{_command} = $message->{_body}->{command}
        or die 'command required';
    $self->{_id} = $message->{_body}->{id}
        or die 'id required';
    $self->{_host} = $message->{_body}->{host} || 'localhost';
    $self->{_tags} = $message->{_body}->{tags} || [];

    my $process = Emux::Process->new(
        id     => $self->{_id},
        host   => $self->{_host},
        on_run => sub {
            exec "/usr/bin/ssh -qt $self->{_host} '$self->{_command}'";
        },
        on_exit => sub {
            my $exit_status = shift;
            # Submit finished message to server.
            $self->server->{_logger}->debug(
                'process %s exited with %s',
                $self->{_id},
                $exit_status,
            );
        }
    );
    $self->proc_manager->run_process($process);

    # register output in server's select loop.
}

1;
