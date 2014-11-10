package Levenshtein;
use base 'Exporter';
our @EXPORT = qw/&levenshtein &fail_fast_lev/;

use List::Util qw/min/;

sub levenshtein {
	my ($s, $t) = @_;
	$_ ||= '' for $s, $t;
	my @n = split //, $s;
	my @m = split //, $t;
#	print "  $s\n";
	my @last = 0..@n;
#	print " ",@last,"\n";
	for my $m (0..$#m) {
		my @new = ($m+1);
		for my $n (0..$#n) {
			my $c = ($n[$n] eq $m[$m]) ? 0 : 1;
			push @new, min(($last[$n+1]+1),
						   ($new[$n]+1),
						   ($last[$n]+$c));
		}
#		print "$m[$m]",@new,"\n";
		@last = @new;
	}
	$last[$#last];
}

sub fail_fast_lev {
	# not sure this works...
	my ($A, $la, $B, $lb, $limit) = @_;
	return 1 if $limit < abs($la - $lb);
	($A, $B, $la, $lb) = ($B, $A, $lb, $la) if $la > $lb;
	my ($i, $j) = (0, 0);
	while ($i < $la and $j < $lb) {
		return 1 if $limit <= 0;
		my $sa = substr $A, $i, 1;
		my $sb = substr $B, $j, 1;
		my $c = $sa cmp $sb;
		$limit-- if $c;
		$i++;
		$j++;
		next unless $c;
		$c > 0 ? $i-- : $j--;
	}
	return 0;
}

1;
