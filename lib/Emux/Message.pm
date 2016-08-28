package Emux::Message;

use strict;
use warnings;

use parent qw(Exporter);
use JSON;

use constant {
    TYPE_EXECUTE      => 'execute',
    TYPE_OUTPUT       => 'output',
    TYPE_STATE        => 'state',
    TYPE_STOP         => 'stop',
    TYPE_ERROR_OUTPUT => 'error_output',
    TYPE_FINISHED     => 'finished',
};

our @constants = qw(
    TYPE_EXECUTE
    TYPE_OUTPUT
    TYPE_STATE
    TYPE_STOP
    TYPE_ERROR_OUTPUT
    TYPE_FINISHED
);

our @EXPORT_OK = (
    @constants
);

our %EXPORT_TAGS = (
    constants => \@constants
);

sub new {
    my ($class, $type, $factory) = @_;

    die 'type parameter is required'
        if not defined $type;

    my $self = {
        _type    => $type,
        _command => $factory ? $factory->create($type) : undef,
        _body    => undef,
    };
    bless $self, $class;

    $self->{_command}->message($self)
        if defined $self->{_command};

    # Set some getter/setters directly in symbol table.
    foreach my $var (qw/type command body/) {
        no strict 'refs';
        *{"$class::$var"} = sub {
            my ($self, $arg) = @_;
            $self->{"_$var"} = $arg if defined $arg;
            return $self->{"_$var"};
        } if not defined *{"$class::$var"}{CODE};
    }

    return $self;
}

sub encode {
    my $self = shift;
    return JSON->new->convert_blessed(1)->encode($self);
}

sub from_handle {
    my ($class, $handle, $factory) = @_;
    my $message = undef;

    if (defined (my $buf = <$handle>)) {
        chomp $buf;
        $message = $class->parse($buf, $factory);
    }

    return $message;
}

#
# Class subroutine to parse a JSON-encoded string and
# return a Message object.
#
sub parse {
    my ($class, $json, $factory) = @_;

    my $decoded = JSON->new->decode($json);
    my $message = $class->new($decoded->{type}, $factory);
    $message->body($decoded);

    return $message;
}

sub TO_JSON {
    my $self = shift;
    return {
        type => $self->{_type},
        %{$self->{_body}}
    };
}

1;
