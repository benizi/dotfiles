#!/usr/bin/perl
$::DEBUG = 0;
use strict;
use warnings;
use Getopt::Long qw/:config pass_through/;
GetOptions(
	'repo=s' => \(my $repo_dir = $ENV{HOME}),
	'patterns=s' => \(my $patterns_file = '$REPO/.hg-tosync'),
	'debug+' => \$::DEBUG,
	'outfile=s' => \(my $outfilename = ''),
) or die 'options';
s/\$REPO/$repo_dir/g for $patterns_file;
die "Not to terminal\n" if !(length $outfilename) and -t 1;
$outfilename = '-' unless length $outfilename;

chdir $repo_dir;

my @in_ex;
{
	local @ARGV = ($patterns_file);
	while (<>) {
		chomp;
		my $pm = s/^([+\-])// ? $1 : '+';
		my @pat;
		if (s/^
			([\{\[\(\<|\/+])
			(.+)
			([\}\]\)\>|\/+])
			/$2/x) {
			@pat = ($_);
		} else {
			for (split /\|/) {
				s/\./\\./g;
				s/\*\*/.\0/g;
				s/\*/[^\/]*/g;
				s/\0/*/g;
				s/\?/[^\/]/g;
				push @pat, "^$_\$";
			}
		}
		push @in_ex, {
			pm => $pm,
			qr => qr/$_/,
		} for @pat;
	}
}

use File::Find;
{
	my @files;
	my %excluded_dirs;
	find {
		wanted => sub {
			return if $File::Find::name eq $repo_dir;
			(my $rel = $File::Find::name) =~ s{^\Q$repo_dir\E/?}{};
			die "GOT: $File::Find::name\n" if /^\.\.?$/;
			my $skip = 1;
			for (@in_ex) {
				$::DEBUG > 2 and print "test{ $rel =~ $$_{qr} }\n";
				next unless $rel =~ $$_{qr};
				last if $$_{pm} eq '-';
				$skip = 0;
				last;
			}
			if ($skip) {
				$excluded_dirs{$File::Find::name}++ if -d;
			} else {
				push @files, $rel;
			}
			$::DEBUG and print "$rel? ", $skip?"skip":"include", "\n";
		},
		preprocess => sub {
			return () if $excluded_dirs{$File::Find::dir};
			@_;
		},
	}, $repo_dir;
	my @tracked;
	for (@files) {
		my @lines = readpipe "hg st -ncm $_";
		next if -d;
		my $is_tracked = @lines;
		next unless $is_tracked;
		push @tracked, $_;
	}
	open my $out, "| tar -jcvf $outfilename -T -";
	print $out "$_\n" for @tracked;
}
