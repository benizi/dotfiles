#!/usr/bin/perl
use strict;
use warnings;
use File::Find;
use Cwd;
use Getopt::Long qw/:config pass_through/;
my $N = 23;
GetOptions(
	'N=i' => \$N,
	'samedev' => \(my $samedev = 0),
) or die 'options';
my @dirs;
@ARGV = grep { not
	-d() ? (push @dirs, $_) :
0 } @ARGV;
die "Bad options: @ARGV\n" if @ARGV;
@dirs = (cwd) if !@dirs;

my @F;
my %exclude;
$exclude{$_}++ for
	map glob($_),
	map { ("/home/bhaskell/$_", "/L/home/bhaskell/$_") }
	qw{.mozilla mail_fu massive_mail_cleanup ripping sda 1[0-9]-*};
my $c = 0;
sub commafy {
	local $_ = shift;
	1 while s/(?<=\d)(?=\d\d\d\b)/,/;
	$_
}
my $printed=0; my $printedN=0;
sub printF {
	return if (time-$printed < 2) and not @_ and @F == $printedN;
	$printed=time;
	$printedN=@F;
	print $_+1, ": ",commafy($F[$_][0]),"\t$F[$_][1]\n" for 0..$#F;
}
my %okay;
$okay{$_}++ for map +(stat)[0], @dirs;
find {
	wanted => sub {
		@F = grep { -f $$_[1] } @F if !($c++ % 10000);
		return unless -f;
		(!$samedev) or $okay{(stat)[0]} or return;
		return if -l;
		return unless @F < $N or $F[0][0] < -s;
		@F = sort { $$a[0] <=> $$b[0] or $$a[1] cmp $$b[1] } @F, [ -s, $File::Find::dir."/$_" ]; 
		shift @F if @F > $N;
		printF;
	},
	preprocess => sub {
		my %b;
		$b{$_}++ for @_;
		my @r = grep !$exclude{$File::Find::dir."/$_"}, @_;
		@r = grep -e, @r;
		my %dev;
		if ($samedev) {
			$dev{$_}++ for grep !$okay{(stat "$File::Find::dir/$_")[0]}, @r;
			@r = grep !$dev{$_}, @r;
		}
		my %a;
		$a{$_}++ for @r;
		warn "Skipped $_\n" for grep !$dev{$_}, grep !$a{$_}, keys %b;
		@r;
	},
}, @dirs;
printF(1);
