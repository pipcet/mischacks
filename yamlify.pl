#!/usr/bin/perl
use YAML::XS;
use Data::Dumper;
use File::Slurp qw(slurp);

# returns, recursively, entries in %$h that aren't in %$h2, or $h if
# $h isn't a hash ref.
sub hdiff {
    my($h, $h2) = @_;
    my $ret;

    unless(ref $h eq "HASH") {
	return ($h eq $h2) ? undef : $h;
    }

    for my $key (keys %$h) {
	my $hd = hdiff($h->{$key}, $h2->{$key});

	$ret->{$key} = $hd if defined $hd;
    }

    return $ret;
}

sub scan_dir {
    my($dir) = @_;
    my $h = { };

    my $dh;
    opendir $dh, $dir;
    while(readdir $dh) {
	my $path = $_;
	next if $path =~ /^\.\.?$/;
	$h->{$path} = scan("$dir/$path");
    }
    close $dh;

    return $h;
}

sub scan_file {
    my($file) = @_;

    my $content;
    eval {
	$content = slurp($file);
	chomp $content;
    };

    return $content;
}

sub scan {
    my($path) = @_;

    return undef if -l $path;
    return scan_dir($path) if -d $path;
    return scan_file($path) if -f $path;

    return undef;
}

my $lastdata;
while(1) {
    my $data = scan(".");

    warn YAML::XS::Dump(hdiff($data, $lastdata));
    $lastdata = $data;
    sleep(1);
}
