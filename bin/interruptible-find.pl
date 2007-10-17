#!/usr/bin/perl
use strict;
use warnings;
use File::Find;
my (@dirs, @dirpred, @pred, @act);
my $usage = <<USAGE;
$0 dirs [options] [predicates]
options:
  -ins/-nosens  PAT are case insensitive by default
  -nochdir      don't change directories
  -numeric      don't look up user/group names
predicates:
  -part PAT     part of the filename
  -name PAT     the filename
  -end  PAT     the end of the filename
  -ext  STR     the file extension
  -nohid        ignore "dot" directories and files
  -exec ARGS ;  execute ARGS on current file ( '_' or '{}' == filename )
USAGE
my $casesens = 1;
my $chdir = 1;
my $numeric = 0;
my $FILE = \1;
push @dirs, shift while @ARGV and $ARGV[0] !~ /^-/;
while (1) {
	$_ = $ARGV[0];
	last if !$_ or !/^-/;
	if (/^--?i(?:n(?:s)?)?$/i or /^--?no-?sens(?:itive)?$/) {
		$casesens = 0;
	} elsif (/^--?no-?chdir$/i) {
		$chdir = 0;
	} elsif (/^--?n(?:um(?:eric)?)?$/i) {
		$numeric = 1;
	} else { last; }
	shift;
}
sub ins { local $_ = shift; $casesens ? (/i/i ? 'i' : '') : (/c/i ? '' : 'i') }
while (defined($_ = shift)) {
	if (/^--?type$/i) {
		my $t = shift;
		die "-type requires an argument\n" unless $t;
		$t =~ /^[fdl]$/ or die "Bad -type: $t\n";
		push @pred, eval "sub { -$t }";
	} elsif (/^--?([ci]?)(part|name|end)$/i) {
		my ($ci, $w) = ($1, $2);
		my $ins = ins($ci);
		my $p = shift;
		die "-$ci$w requires an argument\n" unless defined $p;
		(my $pat = $p) =~ s/\//\\\//g;
		for ($w) {
			$pat = "$pat\$" if /end/i;
			$pat = "^$pat\$" if /name/i;
		}
		my $qr = eval "qr/$pat/$ins";
		$@ and die "Bad regular expression in -$ci$w: $p\n$@\n";
		push @pred, eval "sub { shift =~ /$qr/ }";
	} elsif (/^--?([ci]?)ext$/i) {
		my ($ci) = ($1);
		my $ins = ins($ci);
		my $s = shift;
		die "-${ci}ext requires an argument\n" unless defined $s;
		(my $pat = $s) =~ s/\//\\\//g;
		my $qr = eval "qr/\\.\Q$s\E\$/$ins";
		$@ and die "Bad extension: $s\n$@\n";
	} elsif (/^--?no-?hid(?:den)?$/i) {
		push @$_, eval "sub { shift !~ /^\\./ }" for \@dirpred,\@pred;
	} elsif (/^--?exec$/i) {
		my @args;
		while (defined($_ = shift)) {
			last if /^;$/;
			$_ = $FILE if /^(?:_|\{\})$/;
			push @args, $_;
		}
		die "-exec requires arguments\n" unless @args;
		push @act, sub {
			my $fn = shift;
			my @comm = map { $_ == $FILE ? $fn : $_ } @args;
			return !system { $comm[0] } @comm;
		};
	} elsif (/^--?ls(0?)$/i) {
		push @act, length($1) ? \&ls0 : \&ls;
	} elsif (/^--?print(0?)$/i) {
		push @act, length($1) ? \&print0 : \&printfile;
	} else {
		die "Bad predicate: $_\n";
	}
}
@act = (\&printfile) unless @act;
use POSIX qw/strftime/;
sub ls0 { ls(@_,1); }
sub ls {
	my $fn = shift;
	my $z = shift || 0;
	my $eol = shift ? "\0" : "\n";
	my ($perm, $size, $owner, $group) = (stat $fn)[2,7,4,5];
	$numeric or $owner = getpwuid($owner) || $owner;
	$numeric or $group = getgrgid($group) || $group;
	my $p = '';
	if (-l $fn) {
		$p = 'lrwxrwxrwx';
	} else {
		my $pr = $perm & 07777;
		my $suid = ($pr & 04000) ? !!1 : !1;
		my $sgid = ($pr & 02000) ? !!1 : !1;
		my $stik = ($pr & 01000) ? !!1 : !1;
		$p = join '',
			(-d($fn) ? 'd' : '-'),
			(($pr & 00400) ? 'r' : '-'),
			(($pr & 00200) ? 'w' : '-'),
			(($pr & 00100) ? ($suid ? 'S' : 'x') : ($suid ? 's' : '-')),
			(($pr & 00040) ? 'r' : '-'),
			(($pr & 00020) ? 'w' : '-'),
			(($pr & 00010) ? ($sgid ? 'S' : 'x') : ($sgid ? 's' : '-')),
			(($pr & 00004) ? 'r' : '-'),
			(($pr & 00002) ? 'w' : '-'),
			(($pr & 00001) ? ($stik ? 'T' : 'x') : ($stik ? 't' : '-'));
	}
	print "$p\t$owner\t$group\t$size\t$fn$eol";
}
sub print0 { my $fn = shift; print "$fn\0";
sub printfile { print "$_[0]\n"; }
my %exclude;
my $in_int = 0;
$SIG{INT} = sub {
};
find {
	((!@dirpred) ? () :
		(preprocess => sub {
			map {
				my $d = $_;
				!grep !$_->($d), @dirpred
			} grep { -d } @_;
		})),
	(wanted => sub {
		return if $exclude{$File::Find::dir};
		my $fn = $_;
		return if grep !$_->($fn), @pred;
		$_->($fn) for @act;
	}),
	($chdir ? () : (nochdir => 1)),
}, @dirs;
