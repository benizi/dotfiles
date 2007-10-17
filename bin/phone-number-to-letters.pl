#!/usr/bin/perl -l
use strict;
use warnings;
my (%alph, %num);
my ($i, $n) = (0, 2);
for my $l ('a'..'z') {
	if ($i == 3 and $l !~ /[qz]/) {
		$i = 0;
		$n++;
	}
	$i++ unless $l =~ /[qz]/;
	$alph{$n} .= $l;
	$num{$l} = $n;
}
$_ = "[$_]" for values %alph;
$alph{$_} = "[]" for 0, 1;
print conv($_) for @ARGV;
exit if @ARGV;
print conv($_) while <>;
sub conv {
	local $_ = shift;
	s/[^a-z0-9]//gi;
	/\d/ ? s/(\d)/$alph{$1}/g : s/([a-z])/$num{lc $1}/gei;
	return $_;
}
