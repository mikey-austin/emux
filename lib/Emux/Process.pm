package Emux::Process;

use strict;
use warnings;

sub new {
    my ($class, %args) = @_;
    my $self = {
        _id         => $args{id},
        _host       => $args{host} || 'localhost',
        _on_run     => $args{on_run},
        _on_destroy => $args{on_destroy},
        _pid        => undef,
        _fh         => undef, # The parent filehandle
    };
    bless $self, $class;

    # Set some getters/setters.
    foreach my $var (qw/id host fh pid/) {
        no strict 'refs';
        *{"$class::$var"} = sub {
            my ($self, $arg) = @_;
            $self->{"_$var"} = $arg if defined $arg;
            return $self->{"_$var"};
        } if not defined *{"$class::$var"}{CODE};
    }

    return $self;
}

sub run {
    my $self = shift;
    $self->{_on_run}->($self)
        if defined $self->{_on_run};
}

sub DESTROY {
    my $self = shift;
    $self->{_on_destroy}->($self)
        if defined $self->{_on_destroy};
}

1;
