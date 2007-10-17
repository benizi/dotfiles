#!/usr/bin/perl
use strict;
use warnings;
use Getopt::Long;
use Date::Manip;
Getopt::Long::Configure(qw/bundling/);
use Tk;
my @options =
    ( 'width|w=i' => \ (my $width = 600),
      'height|h=i' => \ (my $height = 600),
      'days=i' => \ (my $radius = 14),
      'grid!' => \(my $draw_grid = 1),
      'ygrid=i' => \ (my $yradius = 10),
      'xtick=i' => \ (my $xtick = 1),
      'xmaj=i' => \ (my $xmaj = 7),
      'ytick=i' => \ (my $ytick = 1),
      'ymaj=i' => \ (my $ymaj = 2),
      'mybirthday|mb=i' => \ (my $birthday = 0),
      'herbirthday|hisbirthday|hb=i' => \ (my $otherbday = 0),
      'roundoff|r' => \ (my $roundoff = 0),
      'minmax' => \(my $min_max = 0),
      );
my $rc_file = "$ENV{HOME}/.config/biorhythms";
if (open my $rc, '<', $rc_file) {
    while (<$rc>) {
        chomp;
        if (/^my=(\d+)$/) {
            $birthday = 0 + $1;
        } elsif (/^([^=]+)=(\d+)$/) {
            my $o = 0 + $2;
            push @options, $1 => sub { $otherbday = $o };
        }
    }
}
GetOptions(@options) or die 'options';
print "$otherbday\n";
$_ = /\D/ ? UnixDate(ParseDate($_),'%s') : UnixDate(ParseDate("epoch $_"), '%s') for grep { $_ } $birthday, $otherbday;

my @types = qw/int emo phy pas wis mas/;
my %colors = qw/int blue emo green phy red pas yellow wis cyan mas magenta/;
my %full = qw/int Intellectual emo Emotional phy Physical
    pas Passion wis Wisdom mas Mastery/;
my %rhythms = qw/phy 23 emo 28 int 33/;
my %comp = qw/mas 23:33 pas 23:28 wis 28:33/;
sub rhythm_days { rhythm($_[0], $_[1]*86_400, $_[2]); }
sub rhythm {
    my ($bday, $x, $which) = @_;
    $which = $rhythms{$which} || $comp{$which} unless $which =~ /^\d+$/;
    return 0 unless $which;
    return (rhythm($bday, $x, $1) + rhythm($bday, $x, $2))/2
        if $which =~ /^(\d+):(\d+)$/;
    sin(($x - $bday)*(3.14159/(43200*$which)));
}

my $mw = MainWindow->new;
my $leg = $mw->Canvas(-width => $width+2, qw/-height 20 -background black/)->pack(qw/-side top/);
$leg->createText($width/(2*@types) + $_ * $width / @types, 10, -fill => $colors{$types[$_]}, -text => $full{$types[$_]}) for 0..$#types;
my $c = $mw->Canvas(-width => $width+2, -height => $height+2, -background => 'black')->pack;
my $t = time;
my $offby = $t % 86400;
$t -= $offby if $roundoff;
print "off $offby".(time % 86400)."\n";
my $whole_t = 2 * 86400 * $radius;
my $half_t  = 86400 * $radius;
my ($center, $major, $minor) = map { "#$_" } qw/eeeeee 999999 666666/;
if ($draw_grid) {
for my $r (0..2 * $radius) {
    my $act = $r - $radius;
    my $xs = $r * $width / (2 * $radius) + 1;
    my $color = '';
    if ($r == $radius) { $color = $center; }
    elsif (not $act % $xtick) { $color = $act % $xmaj ? $minor : $major }
    next unless $color;
    $c->create(line => $xs, 0, $xs, $height, -fill => $color);
}
for my $r (0..2 * $yradius) {
    my $act = $r - $yradius;
    my $ys = $r * $height / (2 * $yradius) + 1;
    my $color = '';
    if ($r == $yradius) { $color = $center; }
    elsif (not $act % $ytick) { $color = $act % $ymaj ? $minor : $major }
    next unless $color;
    $c->create(line => 0, $ys, $width, $ys,  -fill => $color);
}
}
if ($roundoff) {
    my $xs = $width / 2;
    $xs += $offby * $width / (86400 * $radius * 2);
    $c->create(line => $xs, 0, $xs, $height, -fill => '#333399');
}
my %lines;
for my $r (@types) {
    my $color = $colors{$r};
    for my $x (0..$width) {
        my ($y1, $y2);
        my ($yo1, $yo2);
        my $s = $x * $whole_t / $width - $half_t;
        $s += $whole_t / $width;
        $y1 = rhythm($birthday, $s + $t, $r);
        $yo1 = rhythm($otherbday, $s + $t, $r);
        $s += $whole_t / $width;
        $y2 = rhythm($birthday, $s + $t, $r);
        $yo2 = rhythm($otherbday, $s + $t, $r);
        my $c1 = $y1 * $yo1;
        my $c2 = $y2 * $yo2;
        $_ *= - $height / 2 for $y1, $y2, $yo1, $yo2, $c1, $c2;
        $_ += $height / 2 for $y1, $y2, $yo1, $yo2, $c1, $c2;
        ($y1, $y2) = ($c1, $c2) if $otherbday;
        push @{$lines{$r}},
			[ line => $x+1, 1+$y1, $x+2, 1+$y2, -fill => $color ];
        print "$x $y1 $y2\n" unless $x;
    }
}
use List::Util qw/min max/;
if ($min_max) {
	my $num = $#{$lines{$types[0]}};
	for my $x (0..$num) {
		for ([\&min,0],[\&max,1]) {
			my ($mm, $col) = @$_;
			my $y1 = $mm->(map $lines{$_}[$x][2], keys %lines);
			my $y2 = $mm->(map $lines{$_}[$x][4], keys %lines);
			my @line = @{$lines{$types[$col]}[$x]};
			$line[2] = $y1;
			$line[4] = $y2;
			$c->create(@line);
		}
	}
} else {
	for my $v (values %lines) {
		$c->create(@$_) for @$v;
	}
}
$mw->bind('<Key-q>', \&Tk::exit);
MainLoop;
