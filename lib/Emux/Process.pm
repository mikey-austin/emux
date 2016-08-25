package Emux::Process;

use strict;
use warnings;

sub new {
    my ($class, %args) = @_;
    my $self = {
        _id      => $args{id},
        _host    => $args{host} || 'localhost',
        _on_run  => $args{on_run},
        _on_exit => $args{on_exit},
        _pid     => undef,
        _fh      => undef, # The parent filehandle
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
    $self->{_on_run}->(@_)
        if defined $self->{_on_run};
}

sub on_exit {
    my $self = shift;
    $self->{_on_exit}->(@_)
        if defined $self->{_on_exit};
}

1;
