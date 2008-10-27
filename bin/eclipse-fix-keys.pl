#!/usr/bin/perl
use strict;
use warnings;
use feature ':5.10';
use Getotp::Long qw/:config pass_through/;
GetOptions( 'doit' => \(my $doit = 0) ) or die 'options';
my $diff = shift || '/root/eclipse-diff.tar.gz';
for (split /\n/, `tar -Pztf $diff`) {
	if (/\.jar$/) {
		my $old = `unzip -p $_ plugin.xml`;
		my $new = 
	} else {
	}
}
