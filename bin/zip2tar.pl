#!/usr/bin/perl
use strict;
use warnings;
use Archive::Zip;
use Archive::Tar;
use Getopt::Long;
Getopt::Long::Configure(qw/pass_through/);
my $special_thing;
BEGIN { $special_thing = ($0 =~ /img/) ? 1 : 0; }
GetOptions(
	'zipfile=s' => \ (my $zipfile),
	'dirfromname!' => \ (my $dirfromname = $special_thing),
	'flatten' => \ (my $flatten = 0),
	'compress=s' => \ (my $compress = ''),
	'ofile|outfile=s' => \ (my $outfile),
) or die 'options';
$flatten = 1 if $dirfromname;
$zipfile = shift unless $zipfile;
$zipfile or die "Must specify .ZIP file. (Just specify it, or use the --zipfile option.)\n";
if (!$outfile) {
	$outfile = $zipfile;
	$outfile =~ s/\.zip$//;
	$outfile .= '.tar';
	$outfile .= ($compress =~ /bz/i ? '.bz2' : $compress =~ /7z/ ? '.7z' : '';
}

my $zip = Archive::Zip->new($zipfile) or die "Couldn't open zipfile ($zipfile).\n";
my $tar = Archive::Tar->new;
my $pre = '';
if ($dirfromname) {
	$pre = $zipfile;
	$pre =~ s/^.*\///;
	$pre =~ s/\.zip$//i;
}
for my $m (sort { $a->fileName cmp $b->fileName } $zip->members) {
	my $fn = $m->fileName;
	$fn =~ s/^.*\/// if $flatten;
	my $ofn = join '/', grep $_, $pre, $fn;
	my $c = $m->contents;
	next unless defined($c) and length $c;
	$tar->add_data($ofn, $c, {
		mtime => $m->lastModTime,
		mode => 0600,
		uid => $<,
		gid => 0+$(,
	});
}
(my $sh_outfile = $outfile) =~ s/([\ \'\$\`])/\\$1/g;
if ($compress =~ /^7z(?:ip)?/i) {
	open STDOUT, "| 7z a -si $sh_outfile" or die "Couldn't open $outfile: $!\n";
} elsif ($compress =~ /^bz(?:ip)?2?/i) {
	open STDOUT, "| bzip2 > $sh_outfile" or die "Couldn't open $outfile: $!\n";
} else {
	open STDOUT, '>', $outfile or die "Couldn't open $outfile: $!\n";
}
$tar->write(\*STDOUT);
close STDOUT;
my @amtime = (stat $zipfile)[8,9];
utime @amtime, $outfile;
