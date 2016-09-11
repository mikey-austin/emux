package Emux::CommandFactory;

use strict;
use warnings;
use Module::Load qw(load);

use Emux::Message qw(:constants);

use constant {
    TYPES => {
        TYPE_EXECUTE()  => 'Execute',
        TYPE_INPUT()    => 'Input',
        TYPE_PIPELINE() => 'Pipeline',
        TYPE_STATE()    => 'State',
        TYPE_STOP()     => 'Stop',
        TYPE_MUTE()     => 'Mute',
        TYPE_UNMUTE()   => 'Unmute',
    }
};

sub new {
    my ($class, $server) = @_;
    my $self = {
        _server => $server
    };
    bless $self, $class;
}

sub create {
    my ($self, $type) = @_;

    my ($namespace, $class) = ('Emux::Command::', TYPES->{$type});
    die "could not make command of type $type"
        unless $class;

    my $abs_class = "${namespace}${class}";
    load($abs_class);

    return $abs_class->new($type, $self->{_server});
}

1;
