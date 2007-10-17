#!/usr/bin/perl
use strict;
use warnings;
use Getopt::Long;
Getopt::Long::Configure(qw/bundling/);
use Tk;
my @options = (
	'width|w=i' => \(my $width = 0),
	'height|h=i' => \(my $height = 900),
	'ygrid=i' => \(my $yradius = 10),
	'xtick=i' => \(my $xtick = 1),
	'xmaj=i' => \(my $xmaj = 7),
	'ytick=i' => \(my $ytick = 1),
	'ymaj=i' => \(my $ymaj = 2),
	'ignorefirst!' => \(my $ignore_first = 1),
	'roundoff|r' => \(my $roundoff = 0),
	'avg=i' => \(my $running = 0),
);
GetOptions(@options) or die 'options';

my (@max, @min, @data);
use List::Util qw/max min/;
while (<>) {
	chomp;
	my (@vals) = split /\t/;
	for (0..$#vals) {
		push @{$data[$_]}, $vals[$_];
		no warnings 'numeric';
		$max[$_] ||= 0;
		$max[$_] = max $vals[$_], $max[$_];
		$min[$_] ||= 0;
		$min[$_] = min $vals[$_], $min[$_];
	}
}
$width = 1+$#{$data[0]};
my @colors = qw/white blue green red yellow cyan magenta/;

my $mw = MainWindow->new(qw/-width 1000/);
$mw->bind('<Key-q>', sub { Tk::exit(); });
my $c = $mw->Scrolled('Canvas', -scrollbars => 's', -width => $width+2, -height => $height+2, -background => 'black')->pack;
my ($center, $major, $minor) = map { "#$_" } qw/eeeeee 999999 666666/;
sub translate {
	my ($x, $series, $running) = @_;
	$running ||= 0;
	my ($max, $min) = ($max[$series], $min[$series]);
	my $dat = $data[$series];
	my @vals = @$dat[grep { $_ >= 0 and $_ <= $#$dat } $x-$running..$x+$running];
	return 0 unless @vals;
	my $sum = 0; $sum += $_ for @vals;
	my $val = $sum / @vals;
	$val -= $min;
	$height - $val * ($height/($max - $min));
}

for my $r (reverse (($ignore_first ? 1 : 0)..$#data)) {
	my $color = $colors[$r];
	for my $x (0..$width) {
		my ($y1, $y2) = map { translate($_, $r, $running) } $x, $x+1;
		$c->create(line => $x+1, 1+$y1, $x+2, 1+$y2, -fill => $color);
	}
}
#$c->configure(-scrollregion => [ $c->bbox('all') ]);
MainLoop;
