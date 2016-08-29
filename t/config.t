#!/usr/bin/perl

use warnings;
use strict;
use Test::Simple tests => 9;
use Emux::Config;

# Config with defaults.
my $c = Emux::Config->new;

# Test default values;
ok($c->get('daemonize') == 0);
ok(defined $c->get('socket'));
ok($c->get('pidfile') eq '/tmp/emux.pid');

# Test merging.
my %custom = (
    socket    => '/tmp/sock2',
    daemonize => 1
);
$c->merge(\%custom);
ok($c->get('daemonize') == 1);
ok($c->get('socket') eq '/tmp/sock2');
ok($c->get('pidfile') eq '/tmp/emux.pid');

# Test file loading.
$c = Emux::Config->new('t/data/config1.conf');
ok($c->get('daemonize') == 1);
ok(defined $c->get('socket'));
ok($c->get('pidfile') eq '/tmp/pid3');
