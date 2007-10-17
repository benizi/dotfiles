#!/usr/bin/perl
use strict;
use warnings;
use POSIX qw/strftime/;
use Getopt::Long;
Getopt::Long::Configure(qw/pass_through/);
GetOptions(
	'time=s' => \ (my $time = '200611120000'),
	'early=i' => \ (my $early = 9),
	'late=i' => \ (my $late = 21),
	'tzm=i' => \ (my $tzm = 5),
	'tzp=i' => \ (my $tzp = 0),
	'r' => \ (my $recurse = 0),
) or die 'options';
$_ *= 3600 for $early, $late;
my @b_ymdhm = map int, split /(?<=\d\d\d\d)(?=(?:\d\d)*$)/, $time;
$b_ymdhm[0]-=1900;
$b_ymdhm[1]--;
my $bt = strftime "%s", reverse @b_ymdhm, 0;
my $at = time;
my $dt = $at - $bt;
return unless $dt > 3600;
!-d and warn "Not a directory: $_\n" for @ARGV;
while (my $dn = shift @ARGV) {
	next unless -d $dn;
	opendir my $d, $dn or die "opendir $dn\n";
	my @tochange;
	for (map [$_,(stat)[9]], map "$dn/$_", readdir $d) {
		my ($f, $t) = @$_;
		next if $f =~ /\.\.?$/;
		push @ARGV, $f if -d $f and $recurse;
		push @tochange, $_ if $t > $bt;
	}
	close $d;
	my ($min, $max) = (sort { $a <=> $b } map $_->[1], @tochange)[0,-1];
	if (my $delt = $max - $min) {
		for (@tochange) {
			my ($f, $t) = @$_;
			my $fdelt = ($t - $min) / $delt;
			$fdelt = $fdelt**2;
			my $nt = $bt + $dt * $fdelt;
			print "$f: ".localtime($t)." -> ".localtime($nt)."\n";
			touch $nt, $nt, $f;
		}
	}
}
print "@b_ymdhm -> $bt\n";
