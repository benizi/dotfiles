#!/usr/bin/perl
use strict;
use warnings;
use List::Util qw/min/;
use Getopt::Long;
Getopt::Long::Configure(qw/pass_through/);
my $u = "Usage: $0 [options] file1 file2\n";
GetOptions(
	'c=i' => \ (my $context = 32),
	'b=i' => \ (my $before),
	'a=i' => \ (my $align = 16),
) or die $u;
$before = $context/2 unless defined $before;
die $u unless @ARGV==2;
my @f = @ARGV;
$a = do { undef $/; @ARGV=shift(@f); <> };
$b = do { undef $/; @ARGV=shift(@f); <> };
my $l = min map length, $a, $b;
sub lpad { local $_ = shift; ("\0" x ($align - length)).$_ }
sub out { my $o = shift; print lpad(sprintf("%1\$d\n(%1\$x)\n", $o)); $o -= $before; print substr($a,$o,$context); print "-" x ($align - 1); print "\n"; print substr($b,$o,$context); exit; }
for (0..$l-1) {
	out($_) if substr($a,$_,1)ne substr($b,$_,1);
}
if (length($a) != length($b)) { out($l); }
