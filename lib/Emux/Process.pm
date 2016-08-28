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
        _tags    => $args{tags} || [],
        _command => $args{command} || '',
        _created => scalar time,
        _pid     => undef,
        _fh      => undef, # The parent filehandle
        _errors  => undef, # The stderr filehandle
    };
    bless $self, $class;

    # Set some getters/setters.
    foreach my $var (qw/id host fh pid errors created tags command/) {
        no strict 'refs';
        *{"$class::$var"} = sub {
            my ($self, $arg) = @_;
            $self->{"_$var"} = $arg if defined $arg;
            return $self->{"_$var"};
        } if not defined *{"$class::$var"}{CODE};
    }

    return $self;
}

#
# This is run after forking in the child.
#
sub run {
    my $self = shift;
    $self->{_on_run}->(@_)
        if defined $self->{_on_run};
}

#
# This is run in the parent.
#
sub on_exit {
    my $self = shift;
    $self->{_on_exit}->($self, @_)
        if defined $self->{_on_exit};
}

1;
