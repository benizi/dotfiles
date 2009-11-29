#!/usr/bin/perl
use strict;
use warnings;
use charnames qw/:full/;
use open ':std', ':utf8';
unless (eval { require "Unicode/Unihan.pm"; 1 }) {
	eval <<'UNICODY';
package Unicode::Unihan;
use Unicode::UCD 'charscript';
{
	my $warned = 0;
	sub warnonce {
		return if $warned++;
		die "No Unicode::Unihan\n";
	}
}
sub new { bless {}, __PACKAGE__ }
sub unimp { &warnonce; undef }
*Mandarin = \&unimp;
*On = \&unimp;
*Kun = \&unimp;
UNICODY
}
use Unicode::UCD 'charinfo';
my $nonlatin = 0;
my $nonascii = 0;
my $uniq = 0;
@ARGV = grep { not
	/^-*non(lat(?:in)?)?$/i ? ($nonlatin=1) :
	/^-*lat(?:in)?$/i ? ($nonlatin=1) :
	/^-*ascii$/i ? ($nonascii=1) :
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
		my $uni = charinfo(ord);
		my $n = charnames::viacode(ord);
		my $script = $$uni{script} || $$uni{block} || '';
		next if $nonascii and $c =~ /[\x20-\x7e]/;
		next if $nonlatin and $script =~ /^(?:common|latin)$/i;
		my $h = ($script eq 'Han') ? $han->Mandarin($_) : '';
		next if $uniq and $seen{$_}++;
		print join "\t", $c, $x, $n||$h||'(no info)';
		print "\n";
	}
}
