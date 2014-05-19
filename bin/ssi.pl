#!/usr/bin/perl
use strict;
use warnings;
use lib '/home/bhaskell/Usable';
use M;
use Getopt::Long;
my %cmds = (
	echo => [ { var => '' }, \&echo ],
	include => [ { virtual => '' }, \&include ],
);
my $infile;
my @seen;
GetOptions(
	'<>' => sub { $infile = shift; },
	'file=s' => \$infile,
	'basedir=s' => \(my $basedir = ''),
	'baseurl=s' => \(my $baseurl = ''),
	'url=s' => \(my $url = ''),
	'seen=s@' => \@seen,
	'level=i' => \(my $level = 0),
	'max=i' => \(my $max_rec = 4),
) or die 'options';
sub _paramy { 1 while s/\$(\w+)/$ENV{$1}||"(undef)"/; $_ }
die "Excessive recursion (>$max_rec levels)\n" if $level > $max_rec;
my %seen;
$seen{$_}++ for @seen;
$seen{$infile}++;

my $cmdqr = join '|', map quotemeta, reverse sort keys %cmds;
$cmdqr = qr/$cmdqr/;
my $text = do { local@ARGV=($infile); undef local$/; <> };
my @pieces = split /(<!--#\S+\b.*?\s-->)/sm, $text;
for (@pieces) {
	my ($cmd) = /^<!--#(\S+)/;
	warn "Unknown command $cmd\n" if $cmd and $cmd !~ /$cmdqr/;
	print and next unless $cmd and my $info = $cmds{$cmd};
	my ($params, $func) = @$info;
	s/^<!--#\S+//;
	my %opt;
	while (length) {
		last unless s/\s(\w+)=([\'\"])([^\2]+)\2//;
		my ($p, $v) = ($1, $3);
		warn "Bad argument for $cmd: $p\n" and next if !exists $$params{$p};
		$opt{$p} = $v;
	}
	s/^\s-->$//;
	warn "Unparsed: $_\n" if length;
	$_ = _paramy for values %opt;
	$func->(%opt);
}
sub echo {
	my %opt = @_;
	my $var = $opt{var};
	print _paramy "\$$var";
}
sub include {
	my %opt = @_;
	my $file = $opt{file}||$opt{virtual};
	if (defined $file) {
		if ($file =~ /\.shtml$/i) {
			system { $^X } 'perl', $0, $file, map { "--seen", $_ } sort keys %seen;
		} else {
			print read_file($file);
		}
	}
}
