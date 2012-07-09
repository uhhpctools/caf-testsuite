#!/usr/bin/env perl

# borrowed from Chapel 1.1. testing infrastructure
# cleaned up & fixed warnings elicited with 'use strict' and 'use warnings'

use strict;
use warnings;

my $child_pid = undef; # added per "use strict"
my $cmdStatus = undef; # added per "use strict"

#
# $ARGV[0] = "-S"
# $ARGV[1] = <timeout value>
# $ARGV[2...] = command to run
#

$| = 1;					# will flush buffers after write

#setpgrp(0,0);

eval {
    local $SIG{ALRM} = sub { die "alarm" };
    alarm $ARGV[0]; # changed from @ARGV[0], per "use warning"
    if (!defined($child_pid = fork())) {
        print STDERR "timedexec cannot fork child";
        exit(1);
    } elsif ($child_pid == 0) {
        setpgrp(0,0);
        exec @ARGV[1..$#ARGV];
        die "timedexec failed to execute: $1";
    } else {
        waitpid($child_pid, 0);
        if ($? == -1) {
            print STDERR "timedexec failed to execute: $!";
            exit(1);
        } elsif ($? & 127) {
            printf STDERR "timedexec died with signal %d, %s coredump",
                   ($? & 127), ($? & 128) ? 'with' : 'without';
            exit(1);
        } else {
            $cmdStatus = $? >> 8;
        }

    }        
    alarm 0;
};
#die if $@ && $@ ne "alarm"; 
#if ($@ eq "alarm") {
if ($@ =~ /alarm/) {
    print STDERR "timedexec Alarm Clock";
#    $child_pgrp = getpgrp($child_pid);
#    $self_pgrp = getpgrp($$);
    kill (-9, $child_pid);
    exit(4);
} else {
    exit($cmdStatus);
}
