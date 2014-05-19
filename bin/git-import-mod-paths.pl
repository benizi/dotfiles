#!/usr/bin/perl
use strict;
use warnings;
use bytes;
use Getopt::Long;
GetOptions(
	't|transform=s@' => \my @transforms,
	'pre|prepend=s' => \(my $prepend = ''),
	'post|append=s' => \(my $append = ''),
	'remove=s' => \(my $remove = ''),
) or die 'options';

my $ccmd = '';
my @looking_for = ();
my @paths;
sub do_paths {
	for my $p (@paths) {
		local $_ = $$p{path};
		s/\Q$remove\E//;
		for my $t (@transforms) {
			eval $t;
			$@ and warn "Transform{$t} returned: $@";
		}
		print "$$p{action} $$p{mode} $$p{mark} $prepend$_$append\n";
	}
	@paths = ();
	1;
}
while (<>) {
	my ($cmd, @rest) = split;
	do_paths and print and next unless defined $cmd;
	if (grep $cmd eq $_, @looking_for) {
		print;
		if ($cmd eq 'data') {
			my ($len) = @rest;
			local $/ = \$len;
			$_ = <>;
			print;
			$/ = "\n";
			print scalar <>;
		}
		next;
	} elsif ($ccmd eq 'commit') {
		if (/^(?<action>M)\s(?<mode>[0-7]+)\s(?<mark>(?::\d+)|[\da-f]+)\s(?<path>.*)$/) {
			my %m = map {; $_ => (@{$-{$_}})[0] } keys %-;
			push @paths, {%m};
			next;
		}
	}
	do_paths;
	if ($cmd eq 'commit') {
		@looking_for = qw/mark author committer data/;
		print;
	} else {
		@looking_for = ();
		print;
	}
	$ccmd = $cmd;
}
do_paths;
