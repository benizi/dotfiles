#!/usr/bin/perl
use strict;
use warnings;
use Term::ReadKey;
END { ReadMode 0; }
ReadMode 4;
print "foo\n";
sub sysy { print "@_\n"; system { $_[0] } @_; }
while (!defined(ReadKey -1)) {
	my @cmd = ("wrapdsp", "wavplay", "/home/bhaskell/CTU24.wav");
	print "Execing\n";
	sysy(@cmd[1..$#cmd]) and sysy(@cmd) and die "exec\n";
}
