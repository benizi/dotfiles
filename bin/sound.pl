#!/usr/bin/perl
use strict;
use warnings;
$|=1;
use bytes;
use Getopt::Long qw/:config pass_through/;
my %mode;
GetOptions(
	'start=i' => \(my $A = 440),
	'plusminus' => \($mode{pm} = 0),
	'relative' => \($mode{rel} = 0),
	'absolute' => \($mode{abs} = 0),
	'rate=i' => \(my $rate = 44100),
	'note=i' => \(my $note = 11025),
) or die 'options';
die "Too many modes\n" if 1 < grep $_, values %mode;
my $counter;
my $rel = 0;
my $split = $mode{pm} ? '' : undef;
sub note {
	my $pitch = shift;
	warn $pitch, "\n";
	print map chr(127+128*sin((6.28*$_/($rate/$pitch)))), map $counter++, 1..$note;
}
while (<>) {
	for (defined $split ? split $split : split) {
		my $p = $A;
		if ($mode{pm}) {
			if (/[+\-]/) { eval "\$rel$_$_"; next; }
		} elsif ($mode{rel}) {
			next if /[^\-\d]/;
			$rel += $_;
		} elsif ($mode{abs}) {
			next if /\D/;
			$rel = $_;
		} else {
			next if /\D/;
			$rel = 1;
		}
		warn "$rel\n";
		note($A * ((2**(1/12))**$rel));
	}
}
