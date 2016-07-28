package Emux::Config;

use strict;
use warnings;

use JSON;

our $VERSION = '0.1';

sub new {
    my ($class, $config_file) = @_;
    my $self = {
        _files  => [ $config_file || () ],
        _config => {},
    };
    bless $self, $class;

    $self->_set_defaults;
    for (@{$self->{_files}}) {
        my $parsed = $self->_load_config($_);
        $self->merge_config($parsed);
    }

    return $self;
}

#
# Merge the parsed sub-configuration into the parsed
# base configuration.
#
sub merge_config {
    my ($self, $sub_config, $base_config) = @_;

    $base_config ||= $self->{_config};
    foreach my $key (keys %{$sub_config}) {
        # Overwrite all other keys.
        $base_config->{$key} = $sub_config->{$key}
    }
}

sub _set_defaults {
    my $self = shift;

    #
    # Sensible defaults go below to allow operation without
    # specifying a configuration file.
    #
    %{$self->{_config}} = (
        daemonize => 0,
        socket    => '/tmp/emux.sock',
        pidfile   => '/tmp/emux.pid'
    );
}

#
# Recursively load and merge all included configuration.
#
sub _load_config {
    my ($self, $file) = @_;
    my $parsed = $self->_parse_file($file);

    my $sub_config;
    if (defined $parsed->{include} and -d $parsed->{include}) {
        #
        # We have a directory, parse each file within.
        #
        opendir(DIR, $parsed->{include})
            or die "Cannot open config directory $parsed->{include} : $!";

        # Only look for *.conf files.
        my @listing = grep { (/\.(conf)$/) } readdir(DIR);
        closedir DIR;

        # Merge in each file found.
        foreach my $file (@listing) {
            $sub_config = $self->_load_config(
                $parsed->{include} . "/$file");
            $self->merge_config($sub_config, $parsed);
        }
    }
    elsif (defined $parsed->{include} and -e $parsed->{include}) {
        # We have an individual file.
        $sub_config = $self->_load_config($parsed->{include});
        $self->merge_config($sub_config, $parsed);
    }

    return $parsed;
}

sub _parse_file {
    my ($self, $file) = @_;

    die "Config file $file does not exist"
        if not -e $file;

    my $parsed;
    eval {
        local $/;
        open(my $fh, '<', $file);
        my $json = <$fh>;
        $parsed = decode_json($json);
        1;
    } or do {
        my $error = $@;
        die "Error parsing $file: $error" # Rethrow.
            if $error;
    };

    return $parsed;
}

sub get {
    shift->{_config}->{$_[0]};
}

1;
