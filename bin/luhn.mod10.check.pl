#!/usr/bin/perl
use strict;
use warnings;
sub valid {
	local $_ = shift;
	my @d = grep /\d/, split //;
	my $s = 0;
	for my $i (0..$#d) {
		my $d = $d[$i];
		$s += $_ for split //, (($i % 2 == @d % 2) ? ($d * 2) : $d);
	}
	$s =~ /0$/;
}
while (<>) {
	chomp;
	my $l = 16 - tr/0-9/0-9/;
	for my $n (0..10**$l-1) {
		my $insert = sprintf "%0*d", $l, $n;
		my $whole = "$_";
		$whole =~ s/\D/$_/ for split //, $insert;
		print "$whole -> valid\n" if valid($whole);
	}
#	print "$_ -> ", valid($_) ? "valid" : "invalid", $/;
}
