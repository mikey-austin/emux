package Emux::Message;

use strict;
use warnings;

use JSON;

use constant {
    TYPE_ERROR   => 'error',
    TYPE_EXECUTE => 'execute',
};

sub new {
    my ($class, $type, $factory) = @_;
    my $self = {
        _type    => $type,
        _command => $factory ? $factory->create($type) : undef,
        _body    => undef,
    };
    bless $self, $class;

    if (defined $self->{_command}) {
        $self->{_command}->message($self);
    }

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

    # The command knows it's own output format.
    return $self->command->output;
}

1;
