#!/usr/bin/perl

use strict;
use warnings;

use Module::Build;

my $required_mods = {
    'JSON' => 0,
};

my $build = Module::Build->new(
    module_name        => 'Emux',
    dist_name          => 'emux',
    dist_abstract      => 'Execution multiplexer',
    dist_version_from  => 'lib/Emux/Config.pm',
    script_files       => {
        'emux' => 'emux',
    },
);

$build->create_build_script;
