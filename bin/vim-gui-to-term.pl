#!/usr/bin/perl
use strict;
use warnings;
use List::Util qw/sum max min/;

my @options = (
	'88' => \(my $use_88 = 0),
	'256' => \(my $use_256 = 0),
	'help|usage|?' => \(my $run_help = 0),
	'x11=s' => \(my $rgb_txt_file = '/usr/share/X11/rgb.txt'),
	'keep' => \(my $keep_gui = 0),
);
my $usage = <<USAGE;
Usage: $0 [options] [files]
    --256            Use Xterm 256-color mode. [default]
    --88             Use Rxvt-Unicode 88-color mode.
	--x11 FILENAME   Load X11 rgb.txt from FILENAME.
                     [default=$rgb_txt_file]
    --help/-?        Print this message.
USAGE
use Getopt::Long qw/:config pass_through/;
GetOptions(@options) or die $usage;
sub usage { print $usage; @_ and print @_, "\n"; exit }
usage if $run_help;

sub _grey2rgb {
	my ($x, $fs) = @_;
	$x -= 16;
	$x -= $fs ** 3;
	my $level;
	if ($fs == 4) {
		$level = ($x?($x+1):$x) * 23.18181818 + 46.36363636;
	} else {
		$level = $x * 10 + 8;
	}
	($level) x 3
}
sub _x2rgb {
	my ($x, $fs, $use_steps) = @_;
	return _grey2rgb $x, $fs if $x >= ($fs == 4 ? 80 : 232);
	$x -= 16;
	my @rgb;
	while (@rgb < 3) {
		unshift @rgb, $x % $fs;
		$x -= $x % $fs;
		$x /= $fs;
	}
	my @steps = (0, 139, 205, 255);
	map $use_steps?$steps[$_]:($_?$_*40+55:0), @rgb;
}
sub x88torgb  { _x2rgb $_[0], 4, 1 }
sub x256torgb { _x2rgb $_[0], 6, 0 }

my $x11_qr = qr/(?:\000)/;
my %x11;
sub _load_x11colors {
	local @ARGV = grep -f,
	@ARGV or return;
	while (<>) {
		chomp;
		s/\t/\\t/g and die "X11 line:\n$_\n" unless /^ *\d+\s+\d+\s+\d+ *\t+(?:\S+(?:\s\S+)*)$/;
		my ($rgb, $x11) = split /\t\t/;
		for ($rgb) {
			s/^\s+//;
			s/\s+$//;
			s/\s+/ /g;
		}
		my @rgb = split ' ', $rgb;
#		warn "RGB{$rgb = (@rgb)} X11={$x11}\n";
		$x11{lc $x11} = sprintf "#%02x%02x%02x", @rgb;
	}
	$x11_qr = join '|', map quotemeta, reverse sort keys %x11;
	$x11_qr = qr/(?:$x11_qr)/i;
}
_load_x11colors;
my @x256 = (([-200,-200,-200]) x 16, map [x256torgb $_], 16..255);
use Memoize;
sub closest {
	my $hex = shift;
	my @rgb = map hex, ($hex =~ /^#(..)(..)(..)$/);
	my @diff = map color_diff(\@rgb, $x256[$_]), 0..$#x256;
	my $min = (sort { $a <=> $b } @diff)[0];
	$diff[$_] == $min and return $_ for 0..$#x256;
	die "No closest for $hex\n";
}
memoize 'closest';

sub color_diff {
	my ($ca, $cb) = @_;
	my ($Ma, $Mb) = map max(@$_), @_;
	my ($ma, $mb) = map min(@$_), @_;
	my $Va = $Ma;
	my $Vb = $Mb;
	my $Ca = $Ma - $ma;
	my $Cb = $Mb - $mb;
	my $sa = $Ca ? (100 * $Va/$Ca) : 0;
	my $sb = $Cb ? (100 * $Vb/$Cb) : 0;
	#(&color_diff_complex) +
	#(0.4 * (($Va - $Vb) ** 2)) +
	(0.1 * (($sa - $sb) ** 2)) +
	sum map { ($$ca[$_] - $$cb[$_]) ** 2 } 0..2;
}

sub color_diff_complex {
	#@$_ = rgb_to_cielab(@$_) for @_;
	@$_ = rgb_to_hsv(@$_) for @_;
	my ($ca, $cb) = @_;
	my @gamma = (2, 1, 1);
	my @mod = (360, 0, 0);
	my @div = (360, 1, 1);
	sum map {
		my $component = ($$ca[$_] - $$cb[$_]);
		$component %= $mod[$_] if $mod[$_];
		$component /= $div[$_];
		$gamma[$_] * ($component ** 2);
	} 0..2;
}

sub rgb_to_xyz {
	# see: http://www.brucelindbloom.com/index.html?Eqn_RGB_XYZ_Matrix.html
	my @rgb = @_;
	my @M = (
		#[ 0.4124564, 0.3575761, 0.1804375 ],
		#[ 0.2126729, 0.7151522, 0.0721750 ],
		#[ 0.0193339, 0.1191920, 0.9503041 ],
		[ 0.7976748465, 0.1351917082, 0.0313534088 ],
        [ 0.2880402025, 0.7118741325, 0.0000856651 ],
        [ 0.0000000000, 0.0000000000, 0.8252114389 ],
	);
	my @xyz;
	for my $i (0..2) {
		push @xyz, sum map $M[$i][$_] * $rgb[$_], 0..2;
	}
	@xyz;
}

sub conversion_f {
	my $t = shift;
	($t > (6/29)**3)
	? ($t ** (1/3))
	: ((1/3) * (29/6)**2 * $t + 4/29);
}

my ($Xn, $Yn, $Zn) = (0.964, 1, 0.825);

sub xyz_to_cielab {
	my ($x, $y, $z) = @_;
	(
		116 * conversion_f($y/$Yn) - 16,
		500 * (conversion_f($x/$Xn) - conversion_f($y/$Yn)),
		200 * (conversion_f($y/$Yn) - conversion_f($z/$Zn)),
	)
}

sub rgb_to_cielab {
	xyz_to_cielab rgb_to_xyz @_;
}

sub rgb_to_hsv {
	my ($r, $g, $b) = map $_/255, @_;
	my $M = max @_;
	my $m = min @_;
	my $C = $M - $m;
	my $h = 0;
	if ($C) {
		if ($M == $r) {
			$h = ($g - $b) / $C;
		} elsif ($M == $g) {
			$h = ($b - $r) / $C;
		} else {
			$h = ($r - $g) / $C;
		}
	}
	$h %= 6;
	$h *= 60;
	my $V = $M;
	my $L = ($M + $m) / 2;
	my $s = 0;
	if ($C and $V) {
		# HSV
		$s = $C / $V;
		# HSL
		# $s = $C / (1 - abs(2 * $L - 1));
	}
	($h, $s, $V);
}

sub keep {
	$keep_gui ? "@_ " : '';
}

my $type = qr{(?:[bf]g)};
while (<>) {
	s{\bcterm(|$type)=\w+}{}g;
	s{\b(gui(|$type)=(UNDERLINE|BOLD|NONE|[bf]g))\b}{keep($1)."cterm$2=$3"}gie;
	s#\b(gui($type)=($x11_qr))\b#keep($1)."gui$2=".$x11{lc $3}#gie;
	s{\b(gui($type)=(#......))\b}{keep($1)."cterm$2=".closest($3)}ge;
	print;
}
