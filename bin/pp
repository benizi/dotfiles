#!/usr/bin/perl
use strict;
use warnings;
use Getopt::Long qw/:config pass_through/;
GetOptions(
	'inplace' => \(my $in_place = 0),
	'bak=s' => \(my $bak = '.bak'),
) or die 'options';
$^I = $bak if $in_place;
my $indent = 0;
while (<>) {
	s/></>\n</gsm;
	my @lines = split /\n/;
	for (@lines) {
		s{^\s+}{};
		s{^<(?![/?!])}{(" " x $indent++)."<"}e;
		$indent-- if m{^\s*\S.*</\w+(?::\w+)?>};
		$indent-- if m{<[^>]+/>};
		s{^\s*(</\w+(?::\w+)?>)}{(" " x --$indent).$1}e;
	}
	print "$_\n" for @lines;
}
