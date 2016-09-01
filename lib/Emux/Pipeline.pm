package Emux::Pipeline;

use strict;
use warnings;
use parent qw(Emux::Process);

sub new {
    my ($class, %args) = @_;
    my $self = {
        _on_exit  => $args{on_exit},
        _procs    => [],
        _stage    => 0,
        _finished => 0,
    };
    bless $self, $class;

    foreach my $var (qw/id host fh pid errors created tags command/) {
        no strict 'refs';
        *{"${class}::$var"} = sub {
            my ($self, $arg) = @_;
            $self->{_procs}->[$self->{_stage}]->$var($arg);
        } if not defined *{"${class}::$var"}{CODE};
    }

    return $self;
}

sub run {
    my $self = shift;
    $self->{_procs}->[$self->{_stage}]->run
        unless $self->finished;
}

sub next_stage {
    my $self = shift;
    $self->{_finished} = 1
        if ++$self->{_stage} > $#{$self->{_procs}};
}

sub finished {
    shift->{_finished};
}

sub add {
    my ($self, $process) = @_;
    push @{$self->{_procs}}, $process;
}

1;
