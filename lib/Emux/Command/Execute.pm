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

    my @options;
    if ($self->{_host} eq 'localhost') {
        push @options, $self->{_command};
    } else {
        push @options, '/usr/bin/ssh', '-qt';
        if (my $control_path = $self->server->{_config}->get('control_path')) {
            push @options, '-S', $control_path;
        }
        push @options, $self->{_host}, "'$self->{_command}'";
        $self->server->{_logger}->debug('running %s', join ' ', @options);
    }

    my $process = Emux::Process->new(
        id      => $self->{_id},
        host    => $self->{_host},
        command => $self->{_command},
        tags    => $self->{_tags},
        on_run  => sub { exec join ' ', @options },
        on_exit => sub {
            my ($process, $exit_status) = @_;
            $self->server->deregister_process(
                $process, $exit_status);
        }
    );

    $self->server->register_process($process);
}

1;
