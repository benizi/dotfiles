#!/usr/bin/perl
use strict;
use warnings;
use Getopt::Long qw/:config pass_through/;
GetOptions(
	'levels|lev|l=i' => \(my $lev = 0),
	'reverse|rev|r' => \(my $reverse = 0),
) or die 'options';
my ($pl,@pk,$pa);
my (%k,%c);
my @toprint;
sub printy {
	my ($l,$key,$amount,@F) = @_;
	my $kt = $key =~ tr/\t/\t/;
#	$lev + @F + 2;
	my @num = (
		(("") x $l),
		$amount,
		(("") x (1+$lev-$l)),
	);
	my @key = (
		$key,
		(("") x (@F-$kt-1)),
	);
	my @out = $reverse ? (@key, reverse @num) : (@num, @key);
	print join("\t", @out), "\n";
}
while (<>) {
	chomp;
	my ($amount, @F) = split /\t/, $_, -1;
	$lev ||= $#F;
	$lev = $#F + $lev if $lev < 0;
	my $line_key = join "\t", @F;
	my @k = map join("\t",@F[0..$_])||'total', -1..$lev;
#	die map "$_\n", @k;
	$k{$_}{$k[$_]}+=$amount for 0..$#k;
	my $redone = 0;
	while (1) {
		my $printed_any = 0;
		for my $l (reverse 0..$lev) {
			next if !@pk;
			my $pk = $pk[$l];
			my $pval = $k{$l}{$pk[$l]};
			next if $amount == $pa;
			#print "{$amount} != {$pval}\n";
			#print "PK and !Eof\n";
			if ($pk[$l] ne $k[$l]) {
				$printed_any++;
				printy $l, $pk, $pval, @F;
			}
		}
		$redone or printy $lev+1, $line_key, $amount, @F;
		if (eof and not $redone++) {
			@pk = @k and @k = map -1, @pk;
			next;
		}
		last;
	}
#	print "BREAK\n" if $printed_any;
	@pk = @k;
	$pa = $amount;
}
