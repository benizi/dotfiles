#!/usr/bin/perl
use strict;
use warnings;
$|=1;
# Generate some primes simply, then do the following:
# Pick the first prime p := 2
# Pick the p'th prime (2nd = 3). Repeat

use Getopt::Long;
GetOptions(
	'nprimes=i' => \(my $n_primes = 0),
	'max=i' => \(my $max_prime = 0),
	'readp=s' => \(my $initial_list = ''),
) or die 'options';
die "Unknown option(s) (@ARGV)\n" if @ARGV;

my @p;
if ($initial_list) {
	local @ARGV = ($initial_list);
	chomp(@p = <>);
} else {
	@p = (2,3,5,7,11,13,17,19);
}
@p = @p[0..$n_primes-1] if $n_primes and $n_primes < @p;
@p = grep $_<=$max_prime, @p if $max_prime;

print "$_\n" for @p;
my @c = ();
my $l = $p[-1];
prime: while (1) {
	if (!@c) {
		@c = grep $_ % 5, grep $_ % 3, map $l + $_, map 2 * $_, 1..30;
		$l = $c[-1];
	}
	cand: while (my $c = shift @c) {
		last prime if $max_prime and $c > $max_prime;
		my $csq = 1+int sqrt $c;
		divisor: for my $d (@p) {
			last divisor if $d > $csq;
			next divisor if $c % $d;
			next cand;
		}
		push @p, $c;
		print "$c\n";
		last prime if $n_primes and @p >= $n_primes;
	}
}
