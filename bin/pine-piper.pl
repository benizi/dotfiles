#!/usr/bin/perl
use strict;
use warnings;
use File::Temp 'tempfile';
my ($file, $name) = tempfile;
print $file $_ while <STDIN>;
close $file;
push @ARGV, '_TMPFILE_' if @ARGV < 2;
s/_TMPFILE_/$name/g for @ARGV;
system { $ARGV[0] } @ARGV;
unlink $name;
