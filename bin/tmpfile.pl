#!/usr/bin/perl
use strict;
use warnings;
use File::Temp 'tempfile';
my @cmd = @ARGV;
my $stdin;
my @delete; END { unlink for @delete; }
for (@cmd) {
	if (/^_STDIN_$/) {
		unless ($stdin) {
			(my($tmpfile), $stdin) = tempfile( DIR => $ENV{TEMP}||'/tmp' );
			print $tmpfile $_ while <STDIN>;
		}
		$_ = $stdin;
	} else {
	}
}
system { $cmd[0] } @cmd;
