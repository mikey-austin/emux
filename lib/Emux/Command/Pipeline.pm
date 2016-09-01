package Emux::Command::Pipeline;

use strict;
use warnings;
use parent qw(Emux::Command::Execute);

use Emux::Pipeline;

sub execute {
    my $self = shift;

    my $message = $self->message;
    die 'list of commands required'
        unless $message->{_body}->{pipeline}
            and ref $message->{_body}->{pipeline} eq 'ARRAY';

    my $pipeline = Emux::Pipeline->new(
        on_exit => sub {
            my ($pipeline, $exit_status) = @_;
            $self->server->deregister_process(
                $pipeline, $exit_status);

            # Register the next stage in pipeline.
            $pipeline->next_stage;
            $self->server->register_process($pipeline)
                unless $pipeline->finished;

            $self->server->{_logger}->debug(
                'finished pipeline of %d processes',
                $pipeline->{_stage}
            ) if $pipeline->finished;
        }
    );

    foreach my $command (@{$message->{_body}->{pipeline}}) {
        my %args = map { $_ => $command->{$_ } }
            qw(id machine command tags);
        my $process = $self->create_process(\%args);
        $pipeline->add($process);
    }

    $self->server->{_logger}->debug(
        'starting pipeline of %d processes',
        scalar @{$pipeline->{_procs}}
    );

    $self->server->register_process($pipeline);
}

1;
