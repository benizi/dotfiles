#!/usr/bin/perl -l
use strict;
use warnings;
use Getopt::Long qw/:config pass_through/;
GetOptions(
	'base=s' => \(my $B),
	'att|attr=s' => \(my $attr = 'href'),
	'gi|tag=s' => \(my $gi = 'a'),
	'print=s' => \(my $print = '$url'),
) or die 'options';
@ARGV = grep { not
	/^http:/ ? ($B ? (1) : ($B = $_)) :
0 } @ARGV;

my $p;
BEGIN { $p = "HTML::Parser"; }
eval "use $p;";
use URI::URL;

$p->new(start_h => [
	sub{
		my ($this_gi, $att, $text) = @_;
		return unless lc $this_gi eq lc $gi;
		return unless exists $$att{$attr};
		my $toprint = $print;
		my $sub = {
			gi => $this_gi,
			att => $$att{$attr},
			url => url($$att{$attr}, $B)->abs,
			text => $text,
		};
		$toprint =~ s{\$$_}{$$sub{$_}}g for keys %$sub;
		print $toprint
	}, "tag, attr, text"]
)->parse_file($_)for@ARGV;
