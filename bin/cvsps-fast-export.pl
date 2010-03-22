#!/usr/bin/perl
use strict;
use warnings;
use bytes;
binmode STDIN;
binmode STDOUT;
use open ':std', IO => ':bytes';
use Getopt::Long qw/:config pass_through/;
GetOptions(
	'log=s' => \(my $cvsps_log),
	'debug+' => \(my $debug = 0),
) or die <<USAGE;
$0 [options] [filename]
    --log=FILENAME / [filename]  Log file to parse, created by cvsps
	--debug                      Print debugging information
USAGE
@ARGV and $cvsps_log //= shift;
$cvsps_log // die "Need --in FILE\n";

my $f;
if ($cvsps_log eq 'stdin' or $cvsps_log eq '-') {
	$f = \*STDIN;
} else {
	open $f, '<', $cvsps_log or die "<$cvsps_log: $!";
}
sub expect {
	my ($pat, $emptyok) = @_;
	my $sep = undef;
	if ($emptyok and ref $emptyok) {
		$sep = $$emptyok;
		$emptyok = shift;
	}
	local $/ = $sep // "\n";
	local $_ = <$f>;
	chomp;
	if ($debug and $/ ne "\n" or $debug > 1) {
		warn <<R;
READ:{{$/}
$_
}
R
	}
	my $ret = undef;
	my $nope = sub {
		$emptyok or die "Expected: $pat\nGot: {$_}\n".tell($f)."\n";
		undef;
	};
	if (!ref $pat) {
		$_ eq $pat or return &$nope;
		$ret = $pat;
	} elsif ('Regexp' eq ref $pat) {
		/$pat/ or return &$nope;
		$ret = $1 // $_;
	}
	$ret
}
while (!eof $f) {
	my $notdone = expect '---------------------', 1;
	$notdone // last;
	my $rev = expect qr/^PatchSet (\d+)\s$/;
	my $date = expect qr{^Date: (\d\d\d\d/\d\d/\d\d \d\d:\d\d:\d\d)$};
	my $auth = expect qr{^Author: (\S+)$};
	my $branch = expect qr{^Branch: (\w[\w\d\-]*|#CVSPS_NO_BRANCH)$};
	my $tag = expect qr{^Tag: ((?:\(none\)|[\w\d.\-]+)\s(?:\Q**INVALID**\E)?)$};
	my $log = expect qr{\ALog:\n(.+)\Z}sm, \"\nMembers: \n";

	my @files;
	while (defined(my $rcsrev = expect qr{^\t(.+:(?:INITIAL|[\d.]+)->[\d.]+(?:\(DEAD\))?)\s$}, 1)) {
		$rcsrev =~ /^(.+):(INITIAL|[\d.]+)->([\d.]+)((?:\(DEAD\))?)$/
			or die "Bad rcs file name?\n$rcsrev\n";
		push @files, $rcsrev;
	}

	$debug and warn <<REV;
Set($rev) Date($date) Author($auth) Branch($branch) Tag($tag) Log(
$log
) Files(
@files
)
REV
}
