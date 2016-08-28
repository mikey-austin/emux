package Emux::CommandFactory;

use strict;
use warnings;
use Module::Load qw(load);

use Emux::Message qw(:constants);

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
    if ($type eq TYPE_EXECUTE) {
        $class = 'Execute';
    }
    elsif ($type eq TYPE_STATE) {
        $class = 'State';
    }
    elsif ($type eq TYPE_STOP) {
        $class = 'Stop';
    }
    elsif ($type eq TYPE_MUTE) {
        $class = 'Mute';
    }
    elsif ($type eq  TYPE_UNMUTE) {
        $class = 'Unmute';
    }
    else {
        die "could not make command of type $type";
    }

    my $abs_class = "${namespace}${class}";
    load($abs_class);

    return $abs_class->new($type, $self->{_server});
}

1;
