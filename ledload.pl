#!/usr/bin/perl
use POSIX qw(atan);
use Time::HiRes qw(sleep);

while(1) {
    my $loadavg = `cat /proc/loadavg`;
    my ($load5, $load10, $load15) = split ' ', $loadavg;
    my $duration = 5.0;
    my $log = 5.0 * log($load5/4.0);
    my $atan = atan($log);
    my $proportion = .5 + $atan/(3.14159);

    system("echo 1 > /sys/class/leds/input3::numlock/brightness");
    print "sleeping for " . ($duration * $proportion) . "\n";
    sleep($duration * $proportion);
    system("echo 0 > /sys/class/leds/input3::numlock/brightness");
    print "sleeping for " . ($duration * (1 - $proportion)) . "\n";
    sleep($duration * (1 - $proportion));
}
