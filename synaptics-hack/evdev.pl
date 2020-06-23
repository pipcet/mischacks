use Data::Dumper;

use POSIX qw(pow atan2);

my $event;
my %types = (0 => "SYN", 1 => "KEY", 2 => "REL", 3 => "ABS");

my $origin_x = undef;
my $origin_y = undef;
my $abs_x = undef;
my $abs_y = undef;
my $rtime;
my $fingers;
my $slot = 0;

$| = 1;

my %key;
my %abs;
my %pos;
my %slot2id;

while (read(STDIN, $event, 24)) {
    my ($time_secs, $time_usecs, undef, $type, $code, $value) = unpack "qllssl", $event;
    my $time = $time_secs + .000001 * $time_usecs;

    my $rtype = $types{$type} // $type;
    if ($rtype eq "SYN") {
        $rtime = $time;
    }
    if ($rtype eq "KEY") {
        $key{$code} = $value;
    } elsif ($rtype eq "ABS") {
        $abs{$code} = $value;
        $slot = $abs{47} // $slot;
        my $id = $slot2id{$slot} = $abs{57} // $slot2id{$slot};
        $val{$id}{$code} = $value;
        $pos{$id}[0] = $value if $code == 53;
        $pos{$id}[1] = $value if $code == 54;
    }

    my $ofingers = $fingers;

    if ($rtype eq "SYN") {
        $fingers = 0;
        @fingers = ();
        my @oids = @ids;
        @ids = grep { $_ != -1 } values %slot2id;
        for my $id (@ids) {
            $fingers++;
            push @fingers, $pos{$id};
        }
        if ($fingers) {
            for my $axis (0, 1) {
                my $sum = 0;
                for my $finger (@fingers) {
                    $sum += $finger->[$axis];
                }
                $sum /= $fingers;
                $pos{0}[$axis] = $sum;
            }
        }

        if (!$long && ($rtime - $origin_t >= .2)) {
            #print "L\n";
            $long = 1;
        }
        if ($ofingers ne $fingers) {
            $hysteresis_x = 0;
            $hysteresis_y = 0;
            $first_direction = undef;
            $origin_x = $pos{0}[0];
            $origin_y = $pos{0}[1];
            if ($ofingers) {
                if ($rtime - $origin_t < .05) {
                    print "V\n" unless $long;
                } else {
                    print "B\n" unless $long;
                }
            }
            $long = 0;
            $tick_origin_t = $origin_t = $rtime;
            print "$fingers\n";

            my @newids = grep { my $id = $_; ! grep { $_ == $id } @oids } @ids;
            if (@newids == 1 && $fingers > 1) {
                my $angle = atan2($pos{$newids[0]}[0] - $pos{0}[0],
                                  $pos{$newids[0]}[1] - $pos{0}[1]);
                my $dir = ($angle + M_PI) * (4 * M_PI);
                my $ns = "";
                my $ew = "";
                if ($pos{$newids[0]}[0] < 300) {
                    $ew = "W";
                } elsif ($pos{$newids[0]}[0] > 600) {
                    $ew = "E";
                }
                if ($pos{$newids[0]}[1] < 368) {
                    $ns = "N";
                } elsif ($pos{$newids[0]}[1] > 368) {
                    $ns = "S";
                }
                print "[$ns$ew]\n";
                print (($pos{$newids[0]}[0] > $pos{0}[0]) ? "+\n" : "-\n");
            }
        }

        #while ($fingers > 1 && ($rtime - $tick_origin_t) > .2) {
        #    print ".\n";
        #    $tick_origin_t += .05;
        #}

        my @quadrants;
        for my $finger (@fingers) {
            my $ns = "";
            my $ew = "";
            if ($finger->[0] < 300) {
                $ew = "W";
            } elsif ($finger->[0] > 600) {
                $ew = "E";
            }
            if ($finger->[1] < 368 ) {
                $ns = "N";
            } elsif ($finger->[1] > 368) {
                $ns = "S";
            }
            push @quadrants, "[$ns$ew]";
        }
        my $quadrants = "[" . join("", sort @quadrants) . "]\n";

        #print $quadrants unless $quadrants eq $lastquadrants;
        $lastquadrants = $quadrants;

        if (!$hysteresis_x && abs($pos{0}[0] - $origin_x) > 40) {
            $hysteresis_x = 1;
            unless (defined $first_direction) {
                $first_direction = ($pos{0}[0] > $origin_x) ? ">" : "<";
                print $first_direction . "\n";
            }
        }
        if (!$hysteresis_y && abs($pos{0}[1] - $origin_y) > 40) {
            $hysteresis_y = 1;
            unless (defined $first_direction) {
                $first_direction = ($pos{0}[1] > $origin_y) ? "v" : "^";
                print $first_direction . "\n";
            }
        }

        %key = ();
        %abs = ();
    }
    #warn "time $time type $rtype code $code value $value\n";
    #warn "$fingers fingers $first_direction $abs_x-$origin_x $abs_y-$origin_y"
     #   if defined $fingers;
}
