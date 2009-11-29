#!/usr/bin/perl
use warnings;
use strict;
use Getopt::Long qw/:config pass_through/;
GetOptions(
	'dry' => \(my $dry = 0),
) or die 'options';
use File::Find;

my @dirs = qw{/usr/www/users/pl721};
if (!@ARGV) {
	local @ARGV = grep -f, "$ENV{HOME}/.prc";
	chomp(@dirs = <>) if @ARGV;
} else {
	@dirs = @ARGV;
}

my @inc;
my @exc;
for (@dirs) {
	push @exc, $_ and next if s/^-//;
	push @inc, $_ and next if s/^\+?//;
}
my $exclude = join '|', map quotemeta, reverse sort @exc;
$exclude = @exc ? qr{^($exclude)(/|$)} : qr/^$/;


# chdir;
# opendir my($d), "." or die "Opening home dir: $!";
# for (readdir $d) {
# 	next unless -l;
# 	next unless readlink =~ m{/usr/www/};
# 	push @dirs, readlink;
# }
# close $d;

find sub {
	return unless -e;
	return if grep $_ =~ $exclude, $File::Find::dir, $File::Find::name;
	my $mode = (stat)[2] & 07777;
	my $new = $mode | 0444;
	-d and $new |= 0111;
	return if $new == $mode;
	printf "chmod [%04o->]%04o %s\n", $mode, $new, $File::Find::dir."/".$_;
	return if $dry;
	chmod $new, $_;
}, @inc;
