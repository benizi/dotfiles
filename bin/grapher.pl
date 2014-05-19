#!/usr/bin/perl
use strict;
use warnings;
use Getopt::Long;
Getopt::Long::Configure(qw/bundling/);
use Tk;
my @options = (
	'width|w=i' => \(my $width = 0),
	'height|h=i' => \(my $height = 640),
	'ygrid=i' => \(my $yradius = 10),
	'xtick=i' => \(my $xtick = 1),
	'xmaj=i' => \(my $xmaj = 7),
	'ytick=i' => \(my $ytick = 1),
	'ymaj=i' => \(my $ymaj = 2),
	'ignorefirst!' => \(my $ignore_first = 1),
	'roundoff|r' => \(my $roundoff = 0),
	'avg=i' => \(my $running = 0),
	'combined' => \(my $combined = 0),
);
GetOptions(@options) or die 'options';

my (@data);
use List::Util qw/max min sum/;
while (<>) {
	chomp;
	my (@vals) = split /\t/;
	push @{$data[$_]}, $vals[$_] for 0..$#vals;
}
$ignore_first and shift @data;
my@min=map$_||0,map{min@$_}@data;my$min=min@min;
my@max=map$_||0,map{max@$_}@data;my$max=max@max;
my@delt=map$max[$_]-$min[$_],0..$#data;
#warn map "@$_\n", \@min, \@max, \@delt;
for(0..$#data){($min[$_],$max[$_])=($min,$max)if!$delt[$_] or $combined}
@delt=map$max[$_]-$min[$_],0..$#data;
#warn map "@$_\n", \@min, \@max, \@delt;
$width = 1+$#{$data[0]};
my @colors = qw/white red green blue yellow cyan magenta/;

my $mw = MainWindow->new(qw/-width 1000/);
$mw->bind('<Key-q>', \&Tk::exit);
my $c = $mw->Scrolled('Canvas', -scrollbars => 's', -width => $width+2, -height => $height+2, -background => 'black')->pack;
my ($center, $major, $minor) = map { "#$_" } qw/eeeeee 999999 666666/;
sub translate {
	my ($x, $series, $n) = @_;
	$n ||= 0;
	my ($max,$min,$delt,$dat) = map $$_[$series], \@max, \@min, \@delt, \@data;
	my @vals = @$dat[grep { $_ >= 0 and $_ < @$dat } $x-$n..$x+$n];
	return 0 unless @vals;
	my $val = sum(@vals)/@vals - $min;
	$val /= $delt;
	$val = 1 - $val;
	$height * $val;
}

for my $r (0..$#data) {
	for my $x (0..$width) {
		my ($y1, $y2) = map { translate($_, $r, $running) } $x, $x+1;
		#print "$r $x $y1 $y2\n";
		$c->create(line => $x+1, 1+$y1, $x+2, 1+$y2, -fill => $colors[$r+$ignore_first]);
	}
}
#$c->configure(-scrollregion => [ $c->bbox('all') ]);
MainLoop;
