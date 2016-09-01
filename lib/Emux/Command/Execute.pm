package Emux::Command::Execute;

use strict;
use warnings;
use parent qw(Emux::Command);

use Emux::Process;

sub execute {
    my $self = shift;

    my $message = $self->message;
    my %args = map { $_ => $message->{_body}->{$_ } }
        qw(id machine command tags);
    my $process = $self->create_process(\%args);
    $self->server->register_process($process);
}

sub create_process {
    my ($self, $args) = @_;

    $args->{command}
        or die 'command required';
    $args->{id}
        or die 'id required';
    $args->{machine} ||= 'localhost';
    $args->{tags}    ||= [];

    my @options;
    if ($args->{command} eq 'localhost') {
        push @options, $args->{command};
    } else {
        push @options, '/usr/bin/ssh', '-qt';
        if (my $control_path = $self->server->{_config}->get('control_path')) {
            push @options, '-S', $control_path;
        }
        push @options, $args->{machine}, "'$args->{command}'";
    }

    my $process = Emux::Process->new(
        id      => $args->{id},
        host    => $args->{host},
        command => $args->{command},
        tags    => $args->{tags},
        on_run  => sub { exec join ' ', @options },
        on_exit => sub {
            my ($process, $exit_status) = @_;
            $self->server->deregister_process(
                $process, $exit_status);
        }
    );

    return $process;
}

1;
