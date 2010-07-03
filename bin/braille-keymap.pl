#!/usr/bin/perl
use strict;
use warnings;
use POSIX 'strftime';
use charnames ':full';
use open ':std', ':utf8';
use Getopt::Long;
GetOptions(
	'name=s' => \(my $name = 'braille'),
	'contract=s' => \(my $contract = '|'),
) or die 'options';
my $cap = "\N{BRAILLE PATTERN DOTS-6}";
while (<DATA>) {
	if (1../HEADER/ and not /HEADER/) {
		s{\[date:\s*([^\]]+)\]}{strftime $1, localtime}ge;
		s{(\$\w+)}{no strict 'refs'; eval qq<$1>}ge;
		print;
		next;
	}
	chomp;
	my ($chars, $patterns) = map [split ' '], split /\t/, $_, -1;
	s{^<contract>}{$contract}i for @$chars;
	$_ = "BRAILLE PATTERN ".(/\D/ ? $_ : "DOTS-$_") for @$patterns;
	for my $c (@$chars) {
		for my $p (@$patterns) {
			my $char = chr charnames::vianame $p;
			print qq{$c\t$char\t" $p\n};
			if ($c =~ /^[a-z]$/) {
				print qq{\U$c\E\t$cap$char\t" (capital) + $p\n};
			}
		}
	}
}
__END__
" Maintainer:   Benjamin R. Haskell <vim-braille@benizi.com>
" Last Changed: [date: %Y-%m-%d %H:%M:%S]

let b:keymap_name="$name"

loadkeymap
HEADER
<Space>	BLANK
a	1
b	12
c	14
d	145
e	15
f	124
g	1245
h	125
i	24
j	245
k	13
l	123
m	134
n	1345
o	135
p	1234
q	12345
r	1235
s	234
t	2345
u	136
v	1236
w	2456
x	1346
y	13456
z	1356
'	3
.	256
,	2
;	23
?	236
"	356
[ ] ( ) { }	2356
-	36
&	12346
<contract>ch	16
<contract>sh	146
<contract>st	34
<contract>th	1456
