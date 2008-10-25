#!/usr/bin/perl
use strict;
use warnings;
use File::Find;
use File::Temp;
use File::Spec;
my $only = 0;
my $debug = 0;
my $quiet = 0;
my $hashcom = 0;
my (@include, @exclude);
my ($arg, $ref);
my @dirs = grep { not
	$arg ? (('ARRAY' eq ref $ref) ? (push @$ref, $_) : ($$ref = $_)) :
	/^--?o(?:nly)?$/ ? ($only = 1) :
	/^--?d(?:ebug)?$/ ? (++$debug) :
	/^--?q(?:uiet)?$/ ? (++$quiet) :
	/^--?(?:p(?:ound)?|h(?:ash)?)$/ ? ($hashcom = 1) :
	/^--?e?x(?:clude)?$/ ? ($arg, $ref) = ($_, \@exclude) :
	/^--?in(?:clude)?$/ ? ($arg, $ref) = ($_, \@include) :
0 } @ARGV;
$_ = (/[^A-Za-z0-9\.\-]/ ? qr/$_/ : quotemeta) for @include, @exclude;
my $usage = <<USAGE;
$0 [options] dir1 dir2
   -o/--only       Print something when a file's only in one of dir1, dir2
   -d/--debug      Add debugging information
   --pound/--hash  Exclude #comment lines
   -x/--exclude P  Exclude files that match P
USAGE
die $usage if @dirs != 2;
my @files;
sub only { my ($d, $f) = @_; !$quiet and $only and print "Only in $d: $f\n" }
for (@dirs) {
	my $fulldir = File::Spec->rel2abs($_);
	my @f;
	find {
		preprocess => sub { grep !/^\.\.?$/, @_ },
		wanted => sub {
			$debug > 1 and print "wanted<$_>\n";
			return if $File::Find::name eq $fulldir;
			return if grep $File::Find::name =~ /$_/, @exclude;
			return if @include and not grep $File::Find::name =~ /$_/, @include;
			$debug > 1 and print "PUSH<$File::Find::name>\n";
			push @f, [ File::Spec->catfile($File::Find::dir, $_), (-d)?1:0 ];
		},
	}, $fulldir;
	my $fulldirs = File::Spec->catfile($fulldir, "");
	$$_[0] =~ s{^\Q$fulldirs\E}{} or $$_[0] =~ s{^\Q$fulldir\E}{} for @f;
	push @files, [sort { $$a[0] cmp $$b[0] } @f];
	die "BAD: $$_[0]\nstarts with $fulldir\n" for grep !index($$_[0],$fulldir), @f;
}
use Data::Dumper; $debug and print Dumper \@files;
my @in = map +{ map { $_ => 1 } map $$_[0], @$_ }, @files;
my @isd = map +{ map { $$_[0] => $$_[1] } @$_ }, @files;
while (!grep !@$_, @files) {
	my ($f1, $f2) = (my @fs) = map $files[$_][0][0], 0, 1;
	my ($d1, $d2) = (my @ds) = map $files[$_][0][1], 0, 1;
	my ($F1, $F2) = map File::Spec->catfile($dirs[$_],$fs[$_]), 0, 1;
	$debug > 1 and print "f1=$f1\nf2=$f2\nd1=$d1\nd2=$d2\nF1=$F1\nF2=$F2\n";
	if ($f1 eq $f2 and $d1 eq $d2) {
		# do the diff
		if ($d1) { shift @$_ for @files; next; }
		my ($t1, $t2) = map File::Temp->new(
			qw/TEMPLATE diffurnorcsXXXXXXXX/
		), 0, 1;
		for ([$t1,$F1], [$t2,$F2]) {
			my ($t,$f) =@$_;
			local @ARGV = ($f);
			while (<>) {
				s/(\$\w+):[^\$]*\$/$1\$/g;
				$hashcom and s/^\s*#.*$/#COMMENT\n/s;
				print $t $_;
			}
		}
		my @cmd = (diff => ($quiet ? '-qurL' : '-urL') => $f2 => $t1 => $t2);
		system { $cmd[0] } @cmd;
		shift @$_ for @files;
		next;
	} elsif ($f1 eq $f2) {
		print "$F1 and $F2 are of different types\n";
		my @fr = ($d1) ? (@files) : (reverse @files);
		my $start = File::Spec->catfile($fr[0][0][0],'');
		shift @$_ for @fr;
		@{$fr[0]} = grep !($$_[0] =~ /^\Q$start\E/), @{$fr[0]};
	} else {
		my $cmp = $f1 cmp $f2;
		my ($dir, $file) = ($cmp < 0) ? ($dirs[0], $f1) : ($dirs[1], $f2);
		only($dir, $file);
		my $fr = ($cmp < 0) ? $files[0] : $files[1];
		if ($$fr[0][1]) {
			my $full = File::Spec->catfile($file,'');
			@$fr = grep !($$_[0] =~ /^\Q$full\E/), @$fr;
		}
		shift @$fr;
	}
}
if (grep 0+@$_, @files) {
	my ($dir, $files) = (@{$files[0]})
		? ($dirs[0], $files[0])
		: ($dirs[1], $files[1]);
	my @subdirs = grep $$_[1], @$files;
	for (@subdirs) {
		my $d = File::Spec->catfile($_,'');
		@$files = grep !($$_[0] =~ /^\Q$d\E/), @$files;
	}
	only($dir,$$_[0]) for @$files;
}
