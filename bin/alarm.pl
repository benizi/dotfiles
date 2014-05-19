#!/usr/bin/perl
use strict;
use warnings;
use Term::ReadKey;
END { ReadMode 0; }
ReadMode 4;
print "foo\n";
sub sysy { print "@_\n"; system { $_[0] } @_; }
while (!defined(ReadKey -1)) {
	for my $wrap ([],['wrapdsp'],['aoss']) {
		for my $prog (['wavplay'],['aplay'],
			map ['aplay','-d',$_], qw/jackplug duplexplug output dsp0/) {
			exit if ReadKey -1;
			my @cmd = (@$wrap, @$prog, "/home/bhaskell/sounds/CTU24.wav");
			sysy @cmd and warn "Err: {@cmd} $!";
		}
	}
}
