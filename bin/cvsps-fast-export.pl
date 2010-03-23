#!/usr/bin/perl
use strict;
use warnings;
use bytes;
binmode STDIN;
binmode STDOUT;
use open ':std', IO => ':bytes';
use IPC::Run 'run';
use Time::Local 'timegm';
use Sys::Hostname;
use Getopt::Long qw/:config pass_through/;
my $cvsps_log;
GetOptions(
	'log=s' => \$cvsps_log,
	'longlog' => \(my $log_cvsps_info),
	'hostname=s' => \(my $hostname),
	'cvsroot=s' => \(my $CVSROOT = $ENV{CVSROOT}),
	'module=s' => \(my $module),
	'checkpoint=i' => \(my $checkpoint = 100),
	'debug+' => \(my $debug = 0),
	'<>' => sub {
		my $arg = shift;
		die "Bad argument $arg" if $cvsps_log;
		$cvsps_log = $arg
	},
) or die <<USAGE;
$0 [options] [filename]
    --log=FILENAME / [filename]  Log file to parse, created by cvsps
    --longlog                    Add full cvsps PatchSet to log message
    --hostname=DOMAIN            Hostname for simple email mapping
	--cvsroot=CVSROOT            CVSROOT (defaults to \$CVSROOT)
	--module=module              Relative root
    --debug                      Print debugging information
USAGE
@ARGV and $cvsps_log //= shift;
$cvsps_log // die "Need --in FILE\n";
$CVSROOT // die "Need --cvsroot\n";
$module // die "Need --module\n";
unless ($hostname) {
	warn "Using default hostname: ".hostname."\n";
	sleep 4;
	$hostname = hostname;
}

my $f;
if ($cvsps_log eq 'stdin' or $cvsps_log eq '-') {
	$f = \*STDIN;
} else {
	open $f, '<', $cvsps_log or die "<$cvsps_log: $!";
}
my $full_patchset = '';
sub expect {
	my ($pat, $emptyok) = @_;
	my $sep = undef;
	if ($emptyok and ref $emptyok) {
		$sep = $$emptyok;
		$emptyok = shift;
	}
	local $/ = $sep // "\n";
	local $_ = <$f>;
	$full_patchset .= $_;
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

# TODO - bhaskell - 2010-03-22 - add branch mappings
sub branch_map { 'refs/origin/'.shift }

sub author_map {
	my $cvsauthor = shift;
	($cvsauthor, "$cvsauthor\@$hostname")
}

sub date_map {
	my $date = shift;
	my (@ymd_hms) = split /\D/, $date;
	$ymd_hms[0]-=1900;
	$ymd_hms[1]--;
	timegm(reverse @ymd_hms), "-0000"
}

my $mark = 0;
sub mark {
	print "mark :", ++$mark, "\n";
	$mark;
}

sub data {
	my $data = shift;
	print "data ", length($data), "\n";
	print $data;
	print "\n";
}

# TODO - bhaskell - 2010-03-22 - better path mapping/CVSROOT/module
sub get_cvs {
	my ($path, $rev) = @_;
	my @cmd = qw/cvs -Q/;
	push @cmd, -d => $CVSROOT;
	push @cmd, qw/co -n -kk -p/;
	push @cmd, -r => $rev;
	push @cmd, "$module/$path";
	run [@cmd], '>', \my $file, '2>', \my $err;
	$file
}

sub commit {
	my ($cvsbranch, $cvsauthor, $date, $message, @files) = @_;
	my $branch = branch_map $cvsbranch;
	my ($name, $email) = author_map $cvsauthor;
	my ($epochdate, $tz) = date_map $date;
	my @actions;
	for (@files) {
		if ($$_{deletion}) {
			push @actions, "D $$_{path}";
		} else {
			print "blob\n";
			my $mark = mark;
			data get_cvs $$_{path}, $$_{rev2};
			push @actions, "M 644 :$mark $$_{path}";
		}
	}
	print "commit $branch\n";
	print "committer $name <$email> $epochdate $tz\n";
	data $message;
	print "$_\n" for @actions;
}

my $count = 0;
while (!eof $f) {
	$full_patchset = '';
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
		push @files, { path => $1, rev1 => $2, rev2 => $3, deletion => $4 };
	}

	commit $branch, $auth, $date, $log, @files;
	if ($checkpoint) {
		$count % ($checkpoint * 10) or print "checkpoint\n";
		++$count % $checkpoint or print "progress $count\n";
	}
	$debug and warn <<REV;
Set($rev) Date($date) Author($auth) Branch($branch) Tag($tag) Log(
$log
) Files(
@files
)
REV
}
