#!/usr/bin/perl
use strict;
use warnings;
use Getopt::Long qw/:config pass_through/;
GetOptions(
	'inplace' => \(my $in_place = 0),
	'bak=s' => \(my $bak = '.bak'),
	'tabs' => \(my $tabs = 0),
	'xml' => \(my $is_xml = 0),
	'json' => \(my $is_json = $0 =~ /j/),
) or die 'options';
$^I = $bak if $in_place;
if (!$is_json) {
	my $indent = 0;
	my $space = $tabs ? "\t" : " ";
	my $gi = qr/[\w\-]/;
	while (<>) {
		s/></>\n</gsm;
		my @lines = split /\n/;
		for (@lines) {
			s{^\s+}{};
			s{^<(?![/?!])}{($space x $indent++)."<"}e;
			$indent-- if m{^\s*\S.*</$gi+(?::$gi+)?>};
			$indent-- if m{<[^>]+/>};
			s{^\s*(</$gi+(?::$gi+)?>)}{($space x --$indent).$1}e;
		}
		print "$_\n" for @lines;
	}
} else {
	eval 'use JSON';
	my $json = JSON->new;
	$json->indent(1);
	$json->space_after(1);
	my $print = $json->encode($json->decode(do { undef local $/; <> }));
	$print =~ s{^((?:   )+)}{" " x (length($1)/3)}gem;
	print $print;
}
