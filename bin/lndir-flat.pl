#!/usr/bin/perl
use strict;
use warnings;
use File::Find;
use File::Spec;
use Getopt::Long qw/:config pass_through/;
my $usage = <<USAGE;
Usage: $0 src-dir [dest-dir]
USAGE
GetOptions(
	'srcdir=s' => \(my $src_dir = ''),
	'dstdir=s' => \(my $dst_dir = ''),
) or die $usage;
$src_dir ||= shift;
die "Need src-dir\n$usage" unless $src_dir;
$dst_dir ||= shift || '.';
$_ = File::Spec->rel2abs($_) for $src_dir, $dst_dir;
sub mkdirp { system { 'mkdir' } mkdir => -p => shift }
mkdirp $dst_dir and die "Problem mkdir -p $dst_dir\n";
my %already;
find sub { return unless -l; $already{readlink()}++ }, $dst_dir;
print "ALREADY{$_}\n" for keys %already;
find {
	wanted => sub {
		return unless -f;
		my $fn = $File::Find::name;
		return if $already{$fn};
		$_||='' for my ($base, @ext) = split /\./, $_, 2;
		my $N = 0;
		my $outfn;
		do {
			$outfn = File::Spec->catfile($dst_dir,
				join '.', $base, grep($_, $N++), @ext
			);
		} while (-l $outfn);
		print "symlink $fn => $outfn\n";
		symlink $fn => $outfn;
	},
	preprocess => sub { grep File::Spec->catfile($File::Find::dir, $_) ne $dst_dir, sort @_ },
}, $src_dir;
