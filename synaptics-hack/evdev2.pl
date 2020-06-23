sub command {
    my ($command) = @_;
    if ($command eq "next app") {
        unless (defined $release) {
            system("xdotool keydown shift");
            system("xdotool key F17");
            $release = "xdotool key F20; xdotool keyup shift";
        }
        system("xdotool key shift+F18");
    } elsif ($command eq "prev app") {
        unless (defined $release) {
            system("xdotool keydown shift");
            system("xdotool key F17");
            $release = "xdotool key F20; xdotool keyup shift";
        }
        system("xdotool key shift+F19");
    } elsif ($command eq "next tab") {
        if ($name =~ /^Screen/) {
            system("xdotool key --delay=0 F9 w i n d o w l i s t space minus m Return")
                unless defined $release;
            system("xdotool key Down");
            $release = "xdotool key Return";
            #system("xdotool key ctrl+z ctrl+n");
        } elsif ($name =~ /^screen/) {
            system("xdotool key --delay=0 F9 w i n d o w l i s t space minus m Return")
                unless defined $release;
            system("xdotool key Down");
            $release = "xdotool key Return";
            # system("xdotool key ctrl+z ctrl+p");
        } elsif ($name =~ /Iceweasel$/) {
            system("xdotool key ctrl+Next");
        } else {
            unless (defined $release) {
                system("xdotool key F21");
                $release = "xdotool key --window $window F24";
            }
            system("xdotool key --window $window F22");
        }
    } elsif ($command eq "prev tab") {
        my $name = `xdotool getactivewindow getwindowname`;
        chomp $name;
        if ($name =~ /^Screen/) {
            system("xdotool key --delay=0 F9 w i n d o w l i s t space minus m Return")
                unless defined $release;
            system("xdotool key Up");
            $release = "xdotool key Return";
            # system("xdotool key ctrl+z ctrl+p");
        } elsif ($name =~ /^screen/) {
            system("xdotool key --delay=0 F9 w i n d o w l i s t space minus m Return")
                unless defined $release;
            system("xdotool key Up");
            $release = "xdotool key Return";
            # system("xdotool key ctrl+z ctrl+p");
        } elsif ($name =~ /Iceweasel$/) {
            system("xdotool key ctrl+Prior");
        } else {
            unless (defined $release) {
                system("xdotool key F21");
                $release = "xdotool key --window $window F24";
            }
            system("xdotool key --window $window F23");
        }
    } elsif ($command eq "next frame") {
        unless (defined $release) {
            system("xdotool key F25");
            $release = "xdotool key --window $window F28";
        }
        system("xdotool key --window $window F26");
    } elsif ($command eq "prev frame") {
        unless (defined $release) {
            system("xdotool key F25");
            $release = "xdotool key --window $window F28";
        }
        system("xdotool key --window $window F27");
    } elsif ($command eq "switch") {
        system("xdotool key --window $window F29");
    } elsif ($command eq "up") {
        system("xdotool key --window $window Prior");
    } elsif ($command eq "down") {
        system("xdotool key --window $window Next");
    }
}

while (<>) {
    chomp;
    $string = $_ . $string;

    $string = substr($string, 0, 16);

    print "$string\n";

    if ($string =~ /^0/) {
        system($release) if defined $release;
        $release = undef;
        $mode = undef;
        $window = undef;
        $name = undef;
        $command = undef;
    } elsif ($string =~ /^4/) {
        system("xscreensaver-command -lock");
    } elsif ($string =~ /^5/) {
        system("sudo bash -x /home/pip/git/moremischacks/s2boththendisk");
    } elsif ($string =~ /^2([V+-]|\[.*?\])*1([BL]|\[.*?\])*0/) {
    } elsif ($string =~ /^[v^<>]?[BL][v^<>]?1([VBL]|\[.*?\])*0/) {
    }

    if ($string =~ /^1B[+-]?\[(.*?)\]2[BLv^<>]*1/) {
        my $q = $1;
        if ($q =~ /N/) {
            $mode //= "app";
        } elsif ($q =~ /S/) {
            $mode //= "frame";
        } else {
            $mode //= "tab";
        }
        $command = (($q =~ /E/) ? "next" : "prev") . " $mode";
        if ($q =~ /^[NS]?$/) {
            $command = "switch";
            $command = "up" if $q eq "N";
            $command = "down" if $q eq "S";
        }

        # $window //= `xdotool getactivewindow`;
        chomp $window;
        # $name //= `xdotool getactivewindow getwindowname`;
        chomp $name;
        warn $command;

        command($command);
    } elsif (defined $command && $string =~ s/^\.//) {
        command($command);
    }
}
