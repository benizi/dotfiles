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
	my $l = 16 - length;
	my $add = 0;
	while ($add < 10 ** $l) {
		my $whole = sprintf "$_%0${l}d", $add++;
		print "$whole -> valid\n" if valid($whole);
	}
#	print "$_ -> ", valid($_) ? "valid" : "invalid", $/;
}
