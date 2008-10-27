#!/usr/bin/perl
use strict;
use warnings;
$|=1;
# Generate some primes simply, then do the following:
# Pick the first prime p := 2
# Pick the p'th prime (2nd = 3). Repeat

my @p = (2,3,5,7,11,13,17,19);
print "$_\n" for @p;
my @c = ();
my $l = $p[-1];
my $p = 1;
while (1) {
	while (@p < $p) {
		if (!@c) {
			@c = grep $_ % 5, grep $_ % 3, map $l + $_, map 2 * $_, 1..30;
			$l = $c[-1];
		}
		cand: while (my $c = shift @c) {
			my $csq = 1+int sqrt $c;
			divisor: for my $d (@p) {
				last divisor if $d > $csq;
				next divisor if $c % $d;
				next cand;
			}
			push @p, $c;
			print "$c\n";
		}
	}
	$p++;
	#$p = $p[$p-1];
	#print $p, (@p < 20) ? " (@p)" : "", "\n";
}
