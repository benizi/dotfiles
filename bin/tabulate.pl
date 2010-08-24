#!/usr/bin/perl
use open ':utf8', ':std';
use strict;
use warnings;
use List::Util qw/min max/;
use CGI qw/:standard *tbody/;
use Getopt::Long qw/:config pass_through/;
GetOptions(
	'maxlength=i' => \(my $maxl),
	'max!' => \(my $use_max = 0),
	'ascii!' => \(my $ascii = 1),
	'color!' => \(my $color = 1),
	'sep=s' => \(my $sep = ''),
	'insep=s' => \(my $insep = "\t"),
	'trim!' => \(my $trim_spaces = 1),
	'header' => \(my $has_hdr = 0),
	'hdr=s' => \(my $headers = ''),
	'repeatheader|r' => \(my $rpt_head = 0),
	'html' => \(my $html = 0),
	'texty' => \(my $texty = 0),
	'wrap' => \(my $wrap = 0),
	'class=s' => \(my $classname = ''),
	'stylefile=s' => \(my $stylefile = ''),
	'notable' => \(my $notable = 0),
	'markodd' => \(my $mark_odd = 0),
	'sort' => \(my $do_sort = 0),
	'plain' => \(my $plain = 0),
) or die 'options';
$plain and ($sep, $texty) = ('  ', 1);
my @headers = split /,/, $headers;
@headers and $has_hdr = 1;
$use_max = 1 if defined $maxl;
$maxl //= 20 if $use_max;
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

sub llength { local $_=@_?shift:$_; s{\e\[[\d;]+m}{}g; length }
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
@l = sort @l if $do_sort;
$insep = qr/$insep/ if $insep and $insep =~ /\\/;
$insep = ' ' if $insep eq "\t" and !grep /\t/, @l[0..min 10, $#l];
@l = map [map { ($texty and $use_max and llength()>$maxl)?substr($_,0,$maxl):$_ } split$insep], @l;
my $maxe = max map 0+@$_, @l;
@$_ < $maxe and push @$_, ('') x ($maxe - @$_) for @l;
my @len;
push @len, min grep($use_max, $maxl), max map llength($l[$_][@len]), 0..$#l for 0..$#{$l[0]};
my $form = join $sep, map "%-${_}s", @len;
@headers and splice @l, 0, 0, [@headers];
if ($texty) {
	for (@l) {
		$_ = sprintf $form, @$_;
		s/\s+$// if $trim_spaces;
		$_ .= "\n";
	}
	$l[0] = "\e[7m".uncolor($l[0])."\e[0m" if $color and $has_hdr;
} else {
	for (0..$#l) {
		my $func = ($has_hdr and !$_) ? \&th : \&td;
		my $atts = {};
		$$atts{class} = 'odd' if $mark_odd and $_ % 2;
		$l[$_] = Tr(join '', map $func->($atts, $_), @{$l[$_]});
		if ($has_hdr and !$_) {
			$l[$_] = thead($l[$_]);
		} else {
			$l[$_] = start_tbody.$l[$_] if $_ == 1-($has_hdr ? 0 : 1);
			$l[$_] = $l[$_].end_tbody if $_ == $#l;
		}
		$l[$_] .= "\n";
	}
}
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
