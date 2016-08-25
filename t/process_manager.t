#!/usr/bin/perl

use warnings;
use strict;

use Test::Simple tests => 12;
use Emux::ProcessManager;
use Emux::Process;
use Emux::Config;
use Emux::Logger;

# Config with defaults.
my $c = Emux::Config->new;
my $l = Emux::Logger->new($c);
my $pm = Emux::ProcessManager->new(
    config      => $c,
    logger      => $l,
    master_opts => "-i$ENV{HOME}/.ssh/test",
);

my @masters = $pm->masters;
ok(@masters == 0);
my @procs = $pm->procs;
ok(@procs == 0);

# Test the running of processes.
my ($run, $destroy) = (0, 0);
my $p = Emux::Process->new(
    id => 'my-echo-command',
    on_run => sub {
        $run++;
    },
    on_destroy => sub {
        $destroy++;
    }
);

$pm->run_process($p);
exit;
sleep(1);
ok($run == 1, 'process was run');
ok($destroy == 1, 'process was destroyed');
@masters = $pm->masters;
ok(@masters == 0, 'masters were destroyed');
@procs = $pm->procs;
ok(@procs == 0, 'processes were cleaned up');

my $long_proc1 = Emux::Process->new(
    id => 'my-long-proc1',
    on_run => sub {
        sleep(10000);
    }
);

my $long_proc2 = Emux::Process->new(
    id => 'my-long-proc2',
    on_run => sub {
        sleep(10000);
    }
);

$pm->run_process($long_proc1);
$pm->run_process($long_proc2);

@masters = $pm->masters;
ok(@masters == 1, 'master created correctly');
@procs = $pm->procs;
ok(@procs == 2, 'processes managed correctly');

$pm->stop_process($long_proc1);
sleep(1);
@masters = $pm->masters;
ok(@masters == 1, 'master created correctly');
@procs = $pm->procs;
ok(@procs == 1, 'processes managed correctly');

$pm->stop_process($long_proc2);
sleep(1);
@masters = $pm->masters;
ok(@masters == 0, 'master cleaned up correctly');
@procs = $pm->procs;
ok(@procs == 0, 'processes managed correctly');
