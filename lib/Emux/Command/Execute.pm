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
    $self->{_host} = $message->{_body}->{machine} || 'localhost';
    $self->{_tags} = $message->{_body}->{tags} || [];

    my $cmd;
    if ($self->{_host} eq 'localhost') {
        $cmd = $self->{_command};
    } else {
        $cmd = "/usr/bin/ssh -qt $self->{_host} '$self->{_command}'";
    }

    my $process = Emux::Process->new(
        id     => $self->{_id},
        host   => $self->{_host},
        on_run => sub { exec $cmd; },
        on_exit => sub {
            my ($process, $exit_status) = @_;
            $self->server->deregister_process(
                $process, $exit_status);
        }
    );

    $self->server->register_process($process);
}

1;
