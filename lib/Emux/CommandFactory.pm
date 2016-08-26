package Emux::CommandFactory;

use strict;
use warnings;
use Module::Load qw(load);

sub new {
    my ($class, $server) = @_;
    my $self = {
        _server => $server
    };
    bless $self, $class;
}

sub create {
    my ($self, $type) = @_;

    my ($namespace, $class) = ('Emux::Command::');
    if ($type eq Emux::Message->TYPE_EXECUTE) {
        $class = 'Execute';
    }
    elsif ($type eq Emux::Message->TYPE_OUTPUT) {
        $class = 'Output';
    }
    elsif ($type eq Emux::Message->TYPE_FINISHED) {
        $class = 'Finished';
    }
    else {
        die "could not make command of type $type";
    }

    my $abs_class = "${namespace}${class}";
    load($abs_class);

    return $abs_class->new($type, $self->{_server});
}

1;
