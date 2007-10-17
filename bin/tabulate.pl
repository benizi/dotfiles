#!/usr/bin/perl -CDS
use strict;
use warnings;
use List::Util qw/min max/;
use Getopt::Long qw/:config pass_through/;
GetOptions(
	'maxlength=i' => \(my $maxl = 20),
	'ascii!' => \(my $ascii = 1),
	'color!' => \(my $color = 1),
	'sep=s' => \(my $sep = ''),
	'header' => \(my $has_hdr = 0),
	'repeatheader|r' => \(my $rpt_head = 0),
	'html' => \(my $html = 0),
	'texty' => \(my $texty = 0),
	'wrap' => \(my $wrap = 0),
	'class=s' => \(my $classname = ''),
	'stylefile=s' => \(my $stylefile = ''),
	'notable' => \(my $notable = 0),
	'markodd' => \(my $mark_odd = 0),
	'escape!' => \(my $escape = 1),
) or die 'options';
$texty = 1 if !$html;
my $class = $classname ? qq{ class="$classname"} : '';
my $td = qq{<td$class>};
my $tr_odd = $mark_odd ? qq{<tr class="odd">} : "<tr>";
if ($html and $wrap) {
	my $style = $stylefile ? qq{<link rel="stylesheet" type="text/css" href="$stylefile" />} : <<'CSS';
<style type="text/css">
table { border-collapse: collapse; }
td { border: 1px solid black; }
</style>
CSS
	print <<HEAD;
<html>
<head>
<title></title>
$style
</head>
<body>
HEAD
}

$sep =~ s/([\$\`])/\\$1/g if $texty;
$sep = eval "qq\x00$sep\x00" if $sep =~ /[\\\$]/;
if (!length $sep) {
	if ($texty) {
		$sep = $ascii ? "\x{2551}" : ' ';
		$sep = "\e[31m$sep\e[0m" if $color;
	} else {
		$sep = "</td>$td";
	}
}
chomp(my @l = <>);
@l = map [map { ($texty and length()>$maxl)?substr($_,0,$maxl):$_ } split/\t/], @l;
my $maxe = max map 0+@$_, @l;
@$_ < $maxe and push @$_, ('') x ($maxe - @$_) for @l;
my @len;
push @len, min $maxl, max map length($l[$_][@len]), 0..$#l for 0..$#{$l[0]};
my $form = join $sep, map "%-${_}s", @len;
if ($texty) { $_ = sprintf($form."\n", @$_) for @l; }
else {
	if ($escape) { for (@l) { s/&/&amp;/g, s/>/&gt;/g, s/</&lt;/g for @$_; } }
	$l[$_] = (((1+$_) % 2)?$tr_odd:"<tr>").
		("$td".join($sep, @{$l[$_]}).'</td></tr>'.$/) for 0..$#l;
}
$l[0] = "\e[7m".uncolor($l[0])."\e[0m" if $color and $has_hdr;
sub uncolor { local $_ = shift; s/\e\[[\d;]+m//g; $_ }
if ($has_hdr and $rpt_head) {
	my $head = $l[0];
	my $n = 24;
	splice @l, $_, 0, $head for reverse grep !($_ % $n), 1..$#l;
}
@l = ("<table>\n", @l, "</table>\n") if $html and not $notable;
print for @l;
if ($html and $wrap) {
	print <<FOOT;
</body>
</html>
FOOT
}
