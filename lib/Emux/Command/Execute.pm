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
    if ($args->{machine} eq 'localhost') {
        push @options, $args->{command};
    } else {
        push @options, '/usr/bin/ssh', '-qt';
        if (my $control_path = $self->server->{_config}->get('control_path')) {
            push @options, '-S', $control_path;
        }

        my $command = $args->{command};
        $command =~ s/'/'\\''/g;
        push @options, $args->{machine}, "'$command'";
    }

    my $full_command = join ' ', @options;
    my $process = Emux::Process->new(
        id      => $args->{id},
        host    => $args->{machine},
        command => $args->{command},
        tags    => $args->{tags},
        on_run  => sub { exec $full_command },
        on_exit => sub {
            my ($process, $exit_status) = @_;
            $self->server->deregister_process(
                $process, $exit_status);
        }
    );

    return $process;
}

1;
