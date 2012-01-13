#!/usr/bin/perl
use strict;
use warnings;

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
sub closest {
	my $hex = shift;
	my @rgb = map hex, ($hex =~ /^#(..)(..)(..)$/);
	my @diff = map 0+($rgb[0]-$x256[$_][0])**2+($rgb[1]-$x256[$_][1])**2+($rgb[2]-$x256[$_][2])**2, 0..$#x256;
	my $min = (sort { $a <=> $b } @diff)[0];
	$diff[$_] == $min and return $_ for 0..$#x256;
	die "No closest for $hex\n";
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
