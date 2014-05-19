#!/usr/bin/perl
use strict;
use warnings;
use Getopt::Long qw/:config pass_through/;
GetOptions(
	'letters=i' => \(my $letters = 3),
	'dir=s' => \(my $outdir = 'split'),
	'flush' => \(my $flush = 0),
) or die 'options';
sub uri_escape {
	local $_ = shift;
	s/([^A-Za-z0-9\'\~])/sprintf "%%%02x", ord/ge;
	$_;
}
$flush and $|=1;
use File::Basename;
{
	my @mru;
	sub mru {
		my $fn = shift;
		splice @mru, $_, 1 for grep { $mru[$_] eq $fn } 0..$#mru;
		push @mru, $fn;
		shift @mru while @mru > 75;
	}
	my %files;
	sub op {
		my $string = shift;
		my @p = ($outdir);
		push @p, map uri_escape(substr($string, 0, $_)), 1..$letters;
		if (500 < keys %files) {
			my %mru;
			$mru{$_} = 1 for @mru;
			close($files{$_}) and delete $files{$_} for grep !$mru{$_}, keys %files;
		}
		my $fn = join '/', @p;
		return $files{$fn} if $files{$fn};
		my $d = dirname $fn;
		system("mkdir","-p",$d) if !-d $d;
		open $files{$fn}, '>>', $fn or die ">>$fn: $!";
		$files{$fn};
	}
}

while (<>) {
	chomp;
	my ($key, @data) = split /\t/;
	my $fh = op($key);
	print $fh $_, $/;
	warn "$.\n" if not $. % 10000;
}
