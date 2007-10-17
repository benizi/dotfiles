#!/usr/bin/perl
use strict;
use warnings;
use bytes;
my $debug = 0;
sub debug { $debug and warn @_; 1; }
sub chrs {
	local $_ = shift;
	join ', ', map sprintf("0x%02x", $_), unpack 'C*', $_;
}
sub chrsa { chrs(pack 'C*', @_); }
my %mapn;
my %mapc;
my $qr = '';
while (<DATA>) {
	chomp;
	last if /^#/;
	$_ = hex for my ($w, $u) = split /\t/;
	$mapn{$w} = $u;
	$mapc{pack 'U', $w} = pack 'U', $u;
	$qr .= sprintf "\\x%02x", $w;
}
debug "MAP ".chrs(pack'U',unpack'U',$_)." -> ".chrs($mapc{$_})."\n" for sort keys %mapc;
use Data::Dumper; debug Dumper \%mapc;
for my $k (sort keys %mapc) {
	debug "MAP ".chrs(pack'U',unpack'U0U',$k)." -> ".chrs($mapc{$k})."\n";
}
$qr = qr/[$qr]/;
# UTF-8:
# [\x00-\x7f]
# [\xc0-\xdf][\x80-\xbf]
# [\xe0-\xef][\x80-\xbf][\x80-\xbf]
# [\xf0-\xf7][\x80-\xbf][\x80-\xbf][\x80-\xbf]
sub getu {
	my ($A,$B,$C,$D) = (@_,0,0,0,0);
	if ($A >= 0 and $A <= 0x7f) {
		#(($A & 0x80) == 0x00)
		return (1, $A);
	} elsif ($A >= 0xc0 and $A <= 0xdf
		and $B >= 0x80 and $B <= 0xbf) {
		return (1, $A, $B);
	} elsif ($A >= 0xe0 and $A <= 0xef
		and $B >= 0x80 and $B <= 0xbf
		and $C >= 0x80 and $C <= 0xbf) {
		return (1, $A, $B, $C);
	} elsif ($A >= 0xf0 and $A <= 0xf7
		and $B >= 0x80 and $B <= 0xbf
		and $C >= 0x80 and $C <= 0xbf
		and $D >= 0x80 and $D <= 0xbf) {
		return (1, $A, $B, $C, $D);
	} else {
		return (0, $A);
	}
}
$/ = \4;
my @buf;
do {
	if (@buf < 5) {
		my $in = <>;
		if (defined $in) {
			push @buf, unpack 'C*', $in;
		}
	}
	last if not @buf;
	my ($is_utf, @c) = getu(@buf);
	splice @buf, 0, scalar @c;
	my $inc;
	debug "".($is_utf?"":"NON-")."UTF-8: ".chrsa(@c).($is_utf?sprintf(" == 0x{%04x}", ord pack 'U', unpack 'U0U', pack 'C*', @c):"")."\n";
	if ($is_utf) {
		$inc = pack 'U', unpack 'U0U', pack 'C*', @c;
		debug "Matches something $inc -> $mapc{$inc}\n" and $inc = $mapc{$inc} if exists $mapc{$inc};
	} else {
		if (@c == 1 and $mapn{$c[0]}) {
			$inc = pack 'U', $mapn{$c[0]};
			debug "Matches something\n";
		} else {
			$inc = pack 'C*', @c;
		}
	}
	debug "Out: ".chrs($inc)."\n";
	print $inc;
} while (@buf);
__DATA__
80	20AC
82	201A
83	0192
84	201E
85	2026
86	2020
87	2021
88	02C6
89	2030
8A	0160
8B	2039
8C	0152
8E	017D
91	2018
92	2019
93	201C
94	201D
95	2022
96	2013
97	2014
98	02DC
99	2122
9A	0161
9B	203A
9C	0153
9E	017E
9F	0178
# from http://www.microsoft.com/globaldev/reference/sbcs/1252.mspx
80 = U+20AC : EURO SIGN
82 = U+201A : SINGLE LOW-9 QUOTATION MARK
83 = U+0192 : LATIN SMALL LETTER F WITH HOOK
84 = U+201E : DOUBLE LOW-9 QUOTATION MARK
85 = U+2026 : HORIZONTAL ELLIPSIS
86 = U+2020 : DAGGER
87 = U+2021 : DOUBLE DAGGER
88 = U+02C6 : MODIFIER LETTER CIRCUMFLEX ACCENT
89 = U+2030 : PER MILLE SIGN
8A = U+0160 : LATIN CAPITAL LETTER S WITH CARON
8B = U+2039 : SINGLE LEFT-POINTING ANGLE QUOTATION MARK
8C = U+0152 : LATIN CAPITAL LIGATURE OE
8E = U+017D : LATIN CAPITAL LETTER Z WITH CARON
91 = U+2018 : LEFT SINGLE QUOTATION MARK
92 = U+2019 : RIGHT SINGLE QUOTATION MARK
93 = U+201C : LEFT DOUBLE QUOTATION MARK
94 = U+201D : RIGHT DOUBLE QUOTATION MARK
95 = U+2022 : BULLET
96 = U+2013 : EN DASH
97 = U+2014 : EM DASH
98 = U+02DC : SMALL TILDE
99 = U+2122 : TRADE MARK SIGN
9A = U+0161 : LATIN SMALL LETTER S WITH CARON
9B = U+203A : SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
9C = U+0153 : LATIN SMALL LIGATURE OE
9E = U+017E : LATIN SMALL LETTER Z WITH CARON
9F = U+0178 : LATIN CAPITAL LETTER Y WITH DIAERESIS
