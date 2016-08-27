package Emux::Config;

use strict;
use warnings;

use JSON;
use constant {
    DEFAULTS => {
        daemonize        => 0,
        pidfile          => '/tmp/emux.pid',
        logger           => '',
        socket           => $ENV{EMUX_SOCKET} || '/tmp/emux.sock',
        user             => '',
        group            => '',
        host             => '',
        port             => '',
        listen_stdin     => 0,
        broadcast_stdout => 0,
    }
};

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
        $self->merge($parsed);
    }

    return $self;
}

#
# Merge the parsed sub-configuration into the parsed
# base configuration.
#
sub merge {
    my ($self, $sub_config, $base_config) = @_;

    my %defaults = %{+DEFAULTS};
    $base_config ||= $self->{_config};
    foreach my $key (keys %{$sub_config}) {
        # Overwrite all other keys.
        $base_config->{$key} = $sub_config->{$key}
            if "$sub_config->{$key}" ne "$defaults{$key}";
    }

    return $self;
}

sub _set_defaults {
    my $self = shift;

    #
    # Sensible defaults go below to allow operation without
    # specifying a configuration file.
    #
    my %defaults = %{+DEFAULTS};
    $self->{_config} = \%defaults;
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
            $self->merge($sub_config, $parsed);
        }
    }
    elsif (defined $parsed->{include} and -e $parsed->{include}) {
        # We have an individual file.
        $sub_config = $self->_load_config($parsed->{include});
        $self->merge($sub_config, $parsed);
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
    my ($self, @keys) = @_;
    my @output;
    push @output, $self->{_config}->{$_}
        for @keys;

    return wantarray ? @output : $output[0];
}

1;
