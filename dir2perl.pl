#!/usr/bin/perl
use Digest::SHA qw( sha512_hex );
use File::Slurp qw( slurp );

# emit a perl script that will recreate all the files in @ARGV, but is
# editable as one Emacs buffer.  Warning: if and when an SHA-512
# collision is found, it might be possible to put arbitrary Perl code
# outside of the heredocs that the output script is made out of. Don't
# use on tainted data.

# perl -pe 's/(Function|Address|Type|Closure|Record|TypeMap|Types|ClosureData)(\/|::)(RTypes|Lazy|Libffi)/$3.$2.$1/eg' - renames all files and references to the Function::RTypes class to RTypes::Function etc.

print <<'EOF';
use File::Slurp;
use File::Path qw(mkpath);

sub write_file {
    my ($path, $data) = @_;

    chomp $data;
    mkpath($path);
    rmdir($path);

    File::Slurp::write_file($path, $data);
}

EOF

for my $file (@ARGV) {
  next if -d $file;

  die unless -f $file;

  die if $file =~ /^(\/|\.\.\/)/;
  die if $file =~ /\'/;

  my $data = slurp($file);
  my $shasum = sha512_hex($data);

  print "write_file('$file', <<'$shasum');\n";
  print "$data";
  print "\n$shasum\n";
}
