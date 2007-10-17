#!/usr/bin/perl
use strict;
use warnings;
use bytes;
use File::Basename;
use FindBin qw/$Bin/;
use XML::Twig;
use Getopt::Long;
Getopt::Long::Configure(qw/pass_through/);
GetOptions(
	'verbose+' => \(my $verbose = 0),
) or die 'options';
sub verbose { $verbose < shift and return 1; warn @_, $/; }

my $class = shift;
die "Must specify at least a class name/file [could be .java]\n" unless $class;
#$class = "$ENV{PWD}/$class" if $class =~ s/^\.\/// or $class =~ /^[^\/]/;
my @args = @ARGV;
s/\.(?:java|class)$// for $class;
$class =~ s/\.(?=[^.]+$)/\// until -f "$class.java" or -f "$class.class";
my $java_class = proper_class($class);
(my $class_dir = $java_class) =~ tr|.|/|;
(my $class_base = $class) =~ s/\Q$class_dir\E$//;
$class_base =~ s/\/$//;

my ($proj_dir, $space_dir) = map walk_up([$class, $ENV{PWD}, $Bin], $_), '.classpath', '.metadata';
sub walk_up {
	my ($dirs, $stop) = @_;
	for my $d (ref($dirs)?@$dirs:$dirs) {
		while ($d and $d !~ /^[\/.]$/) {
			for my $s (ref($stop)?@$stop:$stop) {
				return $d if -f "$d/$s" or -d "$d/$s";
			}
			$d = dirname $d;
		}
	}
}
sub proper_class {
	my $class = shift;
	my $fn = (-f "$class.class") ? "$class.class" : "$class.java";
	die "Couldn't find class/java file for $class\n" unless -f $fn;
	if ($fn =~ /\.java$/i) {
		@ARGV = ($fn);
		my $pack = '';
		my $cn = '';
		while (<>) {
			if (/^package\s(.*)\;/) {
				$pack = $1;
				next;
			}
			if (/public(?:\s\S)*\sclass\s(\S+)/) {
				$cn = $1;
				next;
			}
			last if $pack and $cn;
		}
		return ($pack?"$pack.":"").($cn?$cn:basename $class);
	} else {
		@ARGV = ($fn);
		local $/ = \4;
		my $mag = unpack 'N', scalar <>;
		die "Not a valid class file ($fn) magic number $mag (expected: )\n" unless $mag == 3405691582;
		my $ver = unpack 'N', scalar <>;
		verbose 1, "Version $ver";
		$/ = \6; <>; #ignore 6 bytes
		$/ = \2;
		my $len = unpack 'n', scalar <>;
		verbose 1, "LENGTH $len";
		$/ = \$len;
		my $cn = <>;
		$cn =~ tr|/|.|;
		return $cn;
	}
}
verbose 1, "Java class: $java_class\nclass: $class\nclass_base: $class_base\nproj: $proj_dir\nspace: $space_dir";

my @cp = ($class_base);
my @search = ($proj_dir);
my %searched;
while (@search) {
	my $dir = shift @search;
	my $cp_file = "$dir/.classpath";
	verbose 1, "Non-exist? $cp_file" and next unless -f $cp_file;
	verbose 1, "Already $cp_file" and next if $searched{$cp_file}++;
	verbose 1, "Searching $cp_file";
	XML::Twig->new(twig_handlers=>{classpathentry=>sub {
		my $kind = $_->att('kind')||'';
		my $path = $_->att('path')||'';
		-f $path or -d $path or $path =~ s/^(?=\/)/$space_dir/ or $path = "$dir/$path";
		verbose 1, "FOUND $kind\n$path";
		if ($kind eq 'lib' or $kind eq 'output') {
			push @cp, $path;
		} elsif ($kind eq 'src') {
			my @add = walk_up($path,'.classpath');
			verbose 1, "Adding $_" for @add;
			push @search, @add;
		}
	}})->parsefile($cp_file);
	verbose 1, "TO_Search: $_" for @search;
}
my %seen;
verbose 1, "1 $_" for @cp;
# @cp = grep { $a = /\.jar$/i ? !($seen{basename $_}++) : 1; warn "?$a $_\n"; $a } @cp;
@cp = grep { /\.jar$/i ? !($seen{basename $_}++) : 1 } @cp;
verbose 1, "2 $_" for @cp;
%seen = ();
@cp = grep !$seen{$_}++, @cp;
my $cp = join ':', @cp;
my @command = ("java", "-cp", $cp, $java_class, @args);
verbose 1, "$_" for split /:/, $cp;
exec { $command[0] } @command;
die "Couldn't execute: @command\n";
