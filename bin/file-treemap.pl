#!/usr/bin/perl
use strict;
use warnings;
package Fuse;
use FindBin '$Bin';
use File::Basename;
use Fcntl ':mode';
use M;
BEGIN {
	if (!eval "use WordNet; 1") {
		eval <<'COUNTER';
package WordNet;
sub counter {
	my $c = 0;
	my $disp = 'counter';
	my $last = 0;
	for (@_) {
		local $_ = shift;
		if (/display/ and @_) {
			$disp = shift;
		} else {
			die "Unknown counter opt: $_\n";
		}
	}
	return sub {
		my $t = time;
		if ($t - $last > 5) {
			warn "$disp $c\n";
			$last = $t;
		}
		$c++;
	};
}
COUNTER
	}
}
use Getopt::Long qw/:config pass_through/;
GetOptions(
	'debug+' => \(my $debug = 0),
	'retrieve' => \(my $use_stored = 0),
	'store' => \(my $store_it = 0),
	'sfile=s' => \(my $suff = '.stored'),
	'database|db' => \(my $use_db = 0),
	'createdb' => \(my $create_db = 0),
	'dfile=s' => \(my $db_suff = '_data'),
	'mount|mountpoint=s' => \(my $mount_point = '/ft'),
	'file=s' => \(my $file_in),
	'striproot!' => \(my $strip_root = 1),
	'stdin' => \(my $use_stdin = 0),
	'create|createtab|filetab' => \(my $create = 0),
	'directories|dirs|D=s@' => \my @dirs,
	'xdev' => \(my $xdev = 0),
	'fixup' => \(my $fixup = 0),
) or die 'options';
my (%FILES, %DIRS);
my @stat = qw/dev ino mode nlink uid gid rdev size atime mtime ctime blksize blocks/;
my @fields = (readlink => @stat);
if ($create) {
	$fixup++;
	push @dirs, splice @ARGV if @ARGV;
	(@dirs) = ('/') and warn "No dirs specified, using @dirs\n" unless @dirs;
	my $alldevok = 1;
	my %okdev;
	($alldevok, %okdev) = (0, map {; (stat)[0] => 1 } @dirs) if $xdev;
	my $use = "use File::Find;1";
	eval $use or die "ERROR { $use }\n$!";
#	my $lim=7;
	my $counter = WordNet::counter(display=>'finding files'=>persec=>);
	my $memfile = '';
	open my $file, '+<', \$memfile;
	File::Find::find({
		wanted => sub {
			my $fn = $File::Find::name;
			my %stat;
			$stat{readlink} = (-l) ? (readlink) : '';
			@stat{@stat} = lstat;
			if ($alldevok or $okdev{$stat[0]}) {
				my $dest = S_ISDIR($stat{mode}) ? \%DIRS : \%FILES;
				$$dest{$fn} = join "\t", @stat{@fields};
			}
			$counter->();
		},
#		preprocess => sub { splice @_, $lim if @_ > $lim; @_ },
	}, @dirs);
}
if ($fixup) {
	my %defaults = (
		qw/dev 1 ino 1 mode 0 nlink 1/ =>
		uid => 0+$< => gid => 0+$( =>
		qw/rdev 0 size 4096/ =>
		map(($_ => time), qw/atime mtime ctime/) =>
		qw/blksize 4096 blocks 4096/
	);
	my %file = %defaults;
	my %dir = %defaults;
	$file{mode} = 010000 | 0644;
	$dir{mode} =  004000 | 0755;
	if (!$create) {
		while (<>) {
			chomp;
			my @F = split /\t/, $_, -1;
			my ($fn, %stat);
			if (@F > 2) {
				($fn, @stat{@fields}) = @F;
			} elsif (@F == 2) {
				($fn, @stat{@fields}) = ($F[0], '', @file{@stat});
				$stat{size} = $F[1];
			} elsif (@F == 1) {
				($fn, @stat{@fields}) = ($F[0], '', @dir{@stat});
			} else {
				die "Bad line: $_\n";
			}
			my $dest = S_ISDIR($stat{mode}) ? \%DIRS : \%FILES;
			$$dest{$fn} = join "\t", @stat{@fields};
		}
	}
	my %links;
	for my $fn (map keys %$_, \%FILES, \%DIRS) {
		while ($fn ne "/") {
			$links{dirname $fn}{basename $fn} = 1;
			$fn = dirname $fn;
		}
	}
	$links{$_} = 0 + keys %{$links{$_}} for keys %links;
	$DIRS{$_} //= join "\t", '', @dir{@stat} for keys %links;
	for my $d (sort keys %DIRS) {
		my %stat;
		@stat{@fields} = split "\t", $DIRS{$d}, -1;
		$stat{nlink} = 2 + $links{$d};
		print join("\t", $d, @stat{@fields}), "\n";
	}
	print join("\t", $_, $FILES{$_}), "\n" for sort keys %FILES;
	exit;
}
require "Fuse.pm" or die "$!";
my $do_umount = 1;
sub umount { system { "sudo" } "sudo", "umount", $mount_point; }
umount();
END { $do_umount and umount(); }
$file_in //= shift;
$use_stdin or ($file_in //= "$ENV{HOME}/FT" and warn "Using file: $file_in");
die "File specified ($file_in), but also --stdin\n" if $use_stdin and $file_in;
$file_in = '-' if $use_stdin;
my ($store_f, $db_f);
${$$_[0]} = ($use_stdin ? 'stdin' : $file_in).$$_[1] for [\$store_f,$suff], [\$db_f,$db_suff];
my ($stat, $readlink, $dir);
my $sversion = 1;
if ($use_stored and -f $store_f) {
	my $arr = retrieve $store_f;
	my $fversion = shift @$arr;
	if ($fversion == $sversion) {
		($stat, $readlink, $dir) = @$arr;
	} else {
		warn "Stored file version ($fversion) doesn't match version ($sversion)\n";
	}
} elsif ($use_db) {
	my $use_eval = <<'DBSTUFF';
BEGIN { @AnyDBM_File::ISA = qw{DB_File NDBM_File GDBM_File}; }
use AnyDBM_File;
1;
DBSTUFF
	eval $use_eval or die "Problem with:\n$use_eval\n$!";
	for ([stat=>\$stat],[readlink=>\$readlink],[dir=>\$dir]) {
		my ($base, $ref) = @$_;
		my $fn = join '_', $db_f, $base;
		$$ref = {};
		tie %{$$ref}, AnyDBM_File => $fn or die "Tie $fn: $!";
	}
}
if ($use_db ? (!%$stat) : !$stat) {
	my $counter = WordNet::counter(display => loading => persec =>);
	@ARGV = ($file_in);
	while (<>) {
		chomp;
		my ($fn, $link, @stat) = split /\t/;
		$$stat{$fn} = pack 'N*', @stat;
		$$readlink{$fn} = $link if length $link;
		my $d = dir $fn;
		my $f = base $fn;
		if ($f ne '/') {
			$$dir{$d} = join "\0", map split(/\0/), grep length, grep defined, $$dir{$d}, $f;
		}
		$counter->();
	}
	if ($strip_root) {
		my ($slen, $shortest);
		for (keys %$stat) {
			next if defined $slen and $slen < length;
			$slen = length;
			$shortest = $_;
		}
		$slen--;
		$shortest = substr $shortest, 0, $slen;
		if ($slen and
			!grep substr($_, 0, $slen) ne $shortest, keys %$stat) {
			my @new;
			for my $h ($stat, $readlink, $dir) {
				my $new = {};
				while (my ($k, $v) = each %$h) {
					next if $slen > length $k;
					$_ = substr $k, $slen;
					$_ = '/' unless length;
					$$new{$_} = $$h{$k};
				}
				%$h = %$new;
			}
		}
	}
	nstore [$sversion,$stat,$readlink,$dir], $store_f if $store_it;
}
warn "Mounting\n";
my @funcs;
BEGIN {
	@funcs = (
		'getattr', 'getdir', 'readlink', 'mknod', 'mkdir', 'unlink', 'rmdir', 'symlink',
		'rename', 'link', 'chmod', 'chown', 'truncate', 'utime', 'open', 'read',
		'write', 'statfs', 'flush', 'release', 'fsync', 'setxattr', 'getxattr',
		'listxattr', 'removexattr',
	);
}
use POSIX qw/ENOENT EOPNOTSUPP/;
sub _de { $debug and warn "", (caller(1))[3], "(@_)\n" }
sub getattr { _de(@_); my $fn = shift; return -ENOENT() unless exists $$stat{$fn}; unpack 'N*', $$stat{$fn} }
sub getdir { _de(@_); my $d = shift; '.', '..', map(split(/\0/), grep defined, $$dir{$d}), 0 }
sub readlink { _de(@_); my $fn = shift; $$readlink{$fn}||'' }
sub statfs { _de(@_); -55 }

my $sym = \%::;
$sym = $$sym{$_.'::'} for split /::/, __PACKAGE__;
for my $fn (@funcs) {
	next if exists $$sym{$fn};
	no strict 'refs';
	*$fn = sub { _de(@_); -EOPNOTSUPP() };
}
main(mountpoint=>$mount_point,map { ($_,$_) } @funcs);
