#!/usr/bin/perl -l
use strict;
use warnings;
use Getopt::Long qw/:config pass_through/;
GetOptions(
	'base=s' => \(my $B),
) or die 'options';
@ARGV = grep { not
	/^http:/ ? ($B ? (1) : ($B = $_)) :
0 } @ARGV;

my $p;
BEGIN { $p = "HTML::LinkExtor"; }
eval "use $p;";

$p->new(sub{if("a"eq shift){print for{@_}->{href}}},$B)->parse_file($_)for@ARGV;
