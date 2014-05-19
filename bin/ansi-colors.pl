#!/usr/bin/perl
use strict;
use warnings;
my $notall = 0;
my $text = shift || 'x';
my @attr = 1..7;
my $attr = @attr;
my @comb;
for my $i (0..-1+2**$attr) {
	my $onoff = sprintf "\%0${attr}b", $i;
	next if $onoff =~ /^.1.....$/ and $notall; # \e[2m = unknown
	next if $onoff =~ /^..1....$/ and $notall; # \e[3m = unknown
	next if $onoff =~ /^.....1.$/ and $notall; # \e[6m = unknown
	next if $onoff =~ /^.11....$/;
	next if $onoff =~ /^.....11$/;
	next if $onoff =~ /^...1.1.$/;
	next if $onoff =~ /^...1..1$/;
	next if $onoff =~ /^....1..$/; # blink
	next if $onoff =~ /^...1...$/; # underline
	my $pat = join ';', @attr[grep substr($onoff,$_,1), 0..$#attr];
	$pat ||= 0;
	push @comb, $pat;
}
@comb = sort { ($a=~/7/)<=>($b=~/7/) or $a cmp $b } @comb;
my @fgbg;
for my $fg (30..37) {
	for my $bg (40..47) {
		next if $fg % 10 == $bg % 10;
		push @fgbg, "$fg;$bg";
	}
}
sub header {
	print sprintf "%-5s | ", @_?shift:"";
}
print "Blink: 5; Underline: 4\n";
header("30+"); for my $c (@fgbg) { print substr($c,1,1); } print $/;
header("40+"); for my $c (@fgbg) { print substr($c,4,1); } print $/;
for my $at (@comb) {
	header $at;
	for my $c (@fgbg) {
		print "\e[0;$at;${c}m$text\e[0m";
	}
	print "\n";
}
