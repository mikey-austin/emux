#!/usr/bin/perl

use warnings;
use strict;

use Test::Simple tests => 11;
use POSIX qw(:sys_wait_h);
use Emux::ProcessManager;
use Emux::Process;
use Emux::Config;
use Emux::Logger;
use Time::HiRes;

# Config with defaults.
my $c = Emux::Config->new;
my $l = Emux::Logger->new($c);
my $pm = Emux::ProcessManager->new(
    config      => $c,
    logger      => $l,
    master_opts => "-i$ENV{HOME}/.ssh/test",
);

my @masters = $pm->masters;
ok(@masters == 0, 'initial state ok');
my @procs = $pm->procs;
ok(@procs == 0, 'initial proc state ok');

# Test the running of processes.
my $p = Emux::Process->new(
    id => 'my-echo-command',
    on_run => sub {
        $l->debug('my-echo-command is running');
    },
    on_exit => sub {
        $l->debug('my-echo-command is exiting');
    }
);

# This process finishes by itself.
$pm->run_process($p);
my_sleep(1);
@masters = $pm->masters;
ok(@masters == 0, 'master created correctly');
@procs = $pm->procs;
ok(@procs == 0, 'processes managed correctly');

# These processes need to be killed as they never exit
# by themselves.
my $long_proc1 = Emux::Process->new(
    id => 'my-long-proc1',
    on_run => sub {
        my $dumb = <STDIN>;
    },
    on_exit => sub {
        $l->debug('my-long-proc1 is exiting');
    }
);

my $long_proc2 = Emux::Process->new(
    id => 'my-long-proc2',
    on_run => sub {
        my $from_parent = <STDIN>;
        print "$from_parent\n";
        sleep(1000);
    },
    on_exit => sub {
        $l->debug('my-long-proc1 is exiting');
    }
);

$pm->run_process($long_proc1);
$pm->run_process($long_proc2);

my $message = "you didn't see nothin";
print { $long_proc2->fh } "$message\n";
my $received = readline($long_proc2->fh);
chomp($received);
ok($message eq $received, 'process file descriptors ok');

@masters = $pm->masters;
ok(@masters == 1, 'master reused correctly');
@procs = $pm->procs;
ok(@procs == 2, 'processes managed correctly');

$pm->stop_process($long_proc1);
my_sleep(1);
@masters = $pm->masters;
ok(@masters == 1, 'master preserved correctly');
@procs = $pm->procs;
ok(@procs == 1, 'processes managed correctly');

$pm->stop_process($long_proc2);
my_sleep(1);
@masters = $pm->masters;
ok(@masters == 0, 'master cleaned up correctly');
@procs = $pm->procs;
ok(@procs == 0, 'processes cleaned up correctly');

# Make our own sleep as the builtin has issues with our forking.
sub my_sleep {
    my $end = Time::HiRes::time() + shift();
    for (;;) {
        my $delta = $end - Time::HiRes::time();
        last if $delta <= 0;
        select(undef, undef, undef, $delta);
    }
}
