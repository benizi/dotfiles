#!/usr/bin/perl
use strict;
use warnings;
use File::Find;
use Digest::SHA1 qw/sha1_hex/;
use Getopt::Long;
GetOptions(
	'all' => \(my $all_files = 0),
	'debug+' => \(my $debug = 0),
	'dry' => \(my $dry = 0),
	'sizelimit=s' => \(my $size_limit = 10_000),
) or die 'options';
$debug||=1 if $dry;
my %m = (k=>2**10,m=>2**20,g=>2**30);
$_ = lc for $size_limit;
$size_limit =~ s/^([\d.e]+)(\D)$/exists$m{$2}?$1*$m{$2}:die "Bad size: $1$2"/e;
$size_limit =~ /\D/ and die "Bad size: $size_limit\n";
sub _files_dir {
	my $fn = @_ ? shift : $_;
	(my $files = $fn) =~ s/\.html$/_files/;
	(-d $files) ? $files : undef;
}
sub remove_file_links {
	my $file = shift;
	my $_files = _files_dir $file;
	return shift unless $_files;
	local $_ = shift;
	s/\b$_files//gsm;
	$_;
}
sub read_file {
	my $fn = @_ ? shift : $_;
	open my $f, '<', $fn or die "<$fn: $!";
	do { undef local $/; <$f> };
}

my (%sha1, %done);
use DB_File;
tie %sha1, 'DB_File', "$ENV{HOME}/slogger-sha1.db";
tie %done, 'DB_File', "$ENV{HOME}/slogger-done.db";
END { untie %sha1; }
END { untie %done; }
my $c = 0;
find {
	wanted => sub {
		my $bn = $_;
		return if !-f;
		return if -l;
		return if $size_limit > -s;
		$debug > 1 and exit if ++$c > 10000;
		warn "COUNT $c\n" unless $c % 1000;
		if (my @match = grep -f, map "/home/bhaskell/$_/$bn", qw/pics pt/) {
			pop @match while @match > 1;
			my @s = map -s, $_, @match;
			if ($s[0]!=$s[1]) {
				print "-s($_) != -s(@match)\n";
			} else {
				my $fn = "/home/bhaskell/pics/$_";
				print "Would link $_\nTo $fn\n" if $debug;
				if (not $dry) {
					warn "$_ -> $fn\nError: $!"
						unless unlink and symlink $fn, $_;
				}
			}
		}
		if ($all_files) {
			my $fullfile = "$File::Find::dir/$_";
			return if $done{$fullfile};
			my $txt = read_file;
			my $stripped = remove_file_links($_, $txt);
			my $sha1 = sha1_hex($stripped);
			if (exists $sha1{$sha1}) {
				my $old = $sha1{$sha1};
				warn "Already processed: $old\n" and return
					if $old eq $fullfile;
				$debug and warn ">>\nOLD: $old\nNEW: $fullfile\n<<\n";
				if ($stripped ne $txt) {
					# add link to _files dir from old
					my $dir = _files_dir $old;
					(my $rel = $dir||'') =~ s{^.*/}{};
					$debug and print "ln -s $dir $rel\n";
					if (!$dry and !-l $rel) {
						warn "$dir -> $rel\nErr: $!" unless symlink $dir, $rel;
					}
				}
				$debug and print "ln -s $old $_\n";
				if (!$dry) {
					warn "$old -> $_\nErr: $!"
						unless unlink and symlink $old, $_;
				}
			} else {
				$sha1{$sha1} = $fullfile;
			}
			# print "$sha1\t$_\n";
			$done{$fullfile}=1;
		}
	},
	preprocess => sub {
		map $$_[0],
		sort {
			$$b[1] <=> $$a[1]
			or (!$$a[1] and $$a[3] <=> $$b[3])
			or $$a[0] cmp $$b[0]
		}
		grep { !$$_[1] or $$_[2] > $size_limit }
		map [ $_, (-f)?1:0, (stat)[7,9] ],
		grep !/^\.\.?$/,
		grep { (-d) or ((-f) and $size_limit < -s) }
		@_;
	},
}, $ENV{MOZ5PROF}."/slogger/archive/data";
__END__
$pic="/home/bhaskell/pt/".base; (unlink and symlink("/home/bhaskell/pics/".base,$_)) or warn "Error $_:$!\n" if -f $pic'
