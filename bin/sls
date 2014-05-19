#!/usr/bin/perl
use strict;
use warnings;
my $label = ($ENV{TERM} =~ /screen/) ? "Other" : "Running";
my $current = $ENV{STY};
my @screens;
for (readpipe 'screen -ls') {
	next unless s/^\s+//;
	next unless /\S/;
	my $status = s/\s+\(Attached\)\s*$// ? 'a'
	: s/\s+\(Detached\)\s*$// ? 'd'
	: s/\s+\(Dead\s\?\?\?\)\s*$// ? '!'
	: '?';
	my @parts = split /\./;
	push @screens, [ $status, reverse @parts ];
}
@screens or exit;
my %uniq;
for (@screens) {
	my ($status, @parts) = @$_;
	$uniq{join '.', @parts[0..$_]}++ for 0..$#parts;
}
my @print;
for (sort { join('.', @$a[1..$#$a]) cmp join('.', @$b[1..$#$b]) } @screens) {
	my ($status, @parts) = @$_;
	pop @parts while @parts > 1 and 1 == $uniq{join '.', @parts[0..$#parts-1]};
	my $name = join '.', reverse @parts;
	push @print, [ $status, sprintf "%s:%s", $name, $status ];
}
unshift @print, [ x => "$label screens" ];
my %colors = qw/x 0 a 32 d 34 ! 31 ? 33/;
my $len = 0;
my $block = 10;
my $max = 72;
while (@print) {
	my ($status, $printed) = @{shift @print};
	print "\e[";
	print $colors{$status};
	print "m$printed\e[0m";
	last unless @print;
	$len += length $printed;
	if ($len > $max) {
		print "\n";
		$len = 0;
	} else {
		my $pad = $block - $len % $block;
		print " " x $pad;
		$len += $pad;
	}
}
print "\n";
