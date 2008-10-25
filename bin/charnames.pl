#!/usr/bin/perl
use strict;
use warnings;
use charnames qw/:full/;
use open ':std', ':utf8';
unless (eval { require "Unicode/Unihan.pm"; 1 }) {
	eval <<'UNICODY';
package Unicode::Unihan;
sub new { bless {}, __PACKAGE__ }
sub Mandarin { undef }
sub On { undef }
sub Kun { undef }
UNICODY
}
my $nonlatin = 0;
my $uniq = 0;
@ARGV = grep { not
	/^-*(?:non)?lat(?:in)?$/i ? ($nonlatin=1) :
	/^-*u(?:niq(?:ue)?)?$/i ? (++$uniq) :
0 } @ARGV;
my $han = Unicode::Unihan->new;
my %seen;
while (<>) {
	chomp;
	%seen = () unless $uniq > 1;
	for (split //) {
		(my $c = $_) =~ s/([[:^print:]])/sprintf "\\u%04x", ord $1/ge;
		my $x = sprintf "%04x", ord;
		my $n = charnames::viacode(ord);
		next if $n and $nonlatin and $n =~ /^(?:latin|digit|space)\b/i;
		my $h = $han->Mandarin($_);
		next if $uniq and $seen{$_}++;
		print join "\t", $c, $x, $n||$h||'(no info)';
		print "\n";
	}
}
