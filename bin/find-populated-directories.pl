#!/usr/bin/perl -l
use strict;
use warnings;
use File::Find;
my $p = shift || $ENV{HOME};
my $strip = ($p =~ tr|/|/|);
my %count;
my %filtered;
find {
	wanted => sub {
		my @d = split m|/|, $File::Find::name;
		@d = map join("/", @d[0..$_]), $strip+1..$#d;
		for my $d (@d) {
			if ($count{$d}++ > 10000) {
				print $d unless $filtered{$d}++;
				last;
			}
		}
	},
	preprocess => sub {
		return () if $filtered{$File::Find::dir};
		grep !$filtered{$File::Find::dir."/$_"}, sort @_;
	},
}, $p;
