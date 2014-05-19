#!/usr/bin/perl
use strict;
use warnings;
use File::Find;
my $d = '/usr/portage';
sub desc {
	my $fn = shift;
	open my $f, '<', $fn or die "opening $fn: $!";
	while (<$f>) {
		next unless /^DESCRIPTION="([^"]+)/;
		close $f;
		return $1;
	}
}
my %done;
my $c = 0;
find {
	wanted => sub {
		return unless /\.ebuild$/;
		s/\.ebuild$//, s/^\Q$d\E\///, s/\/[^\/]+$// for my $pack = $File::Find::name;
		return if $done{$pack};
		my $desc = desc($File::Find::name);
		$done{$pack}++;
		print "$pack\t$desc\n";
		warn "$pack\t$desc\n" unless ++$c % 100;
	},
}, $d;
