package Emux::Command;

use strict;
use warnings;

sub new {
    my ($class, $type, $server) = @_;
    bless {
        _type    => $type,
        _message => undef,
        _server  => $server,
    }, $class;
}

sub expand_and_merge_tags {
    my ($self, $ids, $tags) = @_;

    my %ids = map { $_ => 1 } @{$ids};

    my $proc_manager = $self->server->proc_manager;
    my @tagged_ids = $tags
        ? $proc_manager->expand_tags(@{$tags}) : ();

    # Merge tagged ids and supplied ids.
    $ids{$_} = 1 foreach @tagged_ids;
    my @ids = keys %ids;

    return @ids;
}

sub message {
    my ($self, $message) = @_;
    $self->{_message} = $message if defined $message;
    return $self->{_message};
}

sub server {
    my ($self, $server) = @_;
    $self->{_server} = $server if defined $server;
    return $self->{_server};
}

sub execute {
    ...
}

1;
