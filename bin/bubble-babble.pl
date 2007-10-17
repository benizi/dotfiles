#!/usr/bin/perl
use strict;
use warnings;
use Digest::MD5;
use Digest::BubbleBabble qw/bubblebabble/;
use Getopt::Long;
GetOptions(
	'signature' => \ (my $from_sig = 0),
) or die 'options';

my $m = Digest::MD5->new;
while (<>){
	if ($from_sig) {
		chomp;
		tr/://d;
		if (!/^[\da-f]{32}$/) {
			warn "Not a hex signature: $_\n";
			next;
		}
		my $sig = pack 'H32', $_;
		my $bub = bubblebabble(Digest=>$sig);
		print $bub, $/;
	} else {
		$m->add($_);
	}
}
if (!$from_sig) { print bubblebabble(Digest=>$m->digest), $/; }
