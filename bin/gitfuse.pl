#!/usr/bin/perl
use strict;
use warnings;
use bytes;
use Getopt::Long;
GetOptions(
	'mountpoint=s' => \$::mount_point,
	'debug+' => \($::debug = 0),
	'fusedebug' => \$::fusedebug,
	'branchslash=s' => \($::slash = '%'),
	'gitdir=s' => \$::gitdir,
	'allusers' => \$::allusers,
	'notime' => \($::notime = 0),
	'<>' => sub {
		if (!defined $::mount_point) {
			$::mount_point = shift;
		} elsif (!defined $::gitdir) {
			$::gitdir = shift;
		} else {
			die "Already specified --mountpoint and --gitdir\n";
		}
	},
) or die 'options';
$::fusedebug = 1 if $::debug > 2;
defined $::mount_point or die "Need to specify --mountpoint\n";
defined $::gitdir or die "Need to specify --gitdir\n";
use Fuse;
package Fuse;
use Git;
use Data::Dumper;
use IPC::Run 'run';
use Time::Local;
sub umount { run [fusermount=>'-u'=>$::mount_point] }
$SIG{INT} = sub { exit };
$SIG{HUP} = sub { exit };
print "kill -HUP $$\n";
END { umount }
umount;
my $git = Git->repository($::gitdir);
main(
	mountpoint=>$::mount_point,
	grep($::allusers, qw/mountopts allow_other/),
	grep($::fusedebug, qw/debug 1/),
	map {; $_, $_ }
qw/getattr getdir readlink mknod mkdir unlink rmdir symlink
rename link chmod chown truncate utime open read
write statfs flush release fsync setxattr getxattr
listxattr removexattr/);
use POSIX qw/:errno_h ENOENT/;
sub _de { $::debug or return; warn "", (caller(1))[3], "(", map(defined()?$_:"<undef>",@_), ")\n"; }
sub _branches { my @br = $git->command('branch'); s{^..}{} and s{/}{$::slash}g for @br; @br }
my %special;
BEGIN { $special{$_}++ for qw/.index .working .bydate/; }
my %cached_svn_revs;
sub _rev {
	my ($rev, $subst) = @_;
	$subst and $rev =~ s/$::slash/\//g;
	return $rev if $special{$rev};
	my @out;
	if ($rev =~ /\@(r\d+)$/) {
		my $svnrev = $1;
		if (!$cached_svn_revs{$rev}) {
			eval {
				@out = $git->command(['svn','find-rev',$svnrev],{STDERR=>0});
			};
			@out and $cached_svn_revs{$rev} = shift @out;
		}
		@out = $cached_svn_revs{$rev};
	} else {
		eval {
			@out = $git->command(['rev-parse','-q','--verify',$rev],{STDERR=>0});
		};
		return _rev($rev, 1) if !@out and $rev =~ /$::slash/;
	}
	return unless @out;
	shift @out;
}
sub _parts {
	my $fn = shift;
	my @parts = split m{/}, $fn, -1;
	shift @parts;
	my $branch = shift @parts;
	$branch .= '/'.shift(@parts) if $branch eq '.bydate';
	my $dir = join '/', @parts;
	($fn, $branch, $dir)
}
sub _lsfile {
	my $orig_fn = shift;
	my ($fn, $branch, $dir) = _parts $orig_fn;
	my @got;
	$branch = _rev $branch if $branch !~ /^[\da-f]{40}$/;
	for ($git->command('ls-tree', '--long', $branch, '--', $dir)) {
		my ($ptss, $name) = split /\t/;
		if ($name =~ s/^"//) {
			my $orig = qq{"$name};
			$name =~ s/"$//;
			my $newname = '';
			while (length $name) {
				if ($name =~ s/^(\\[nrt]|\\[0-7]{3})//) {
					$newname .= eval qq{qq.$1.};
				} elsif ($name =~ s/^\\(")// || $name =~ s/^(.)//) {
					$newname .= $1;
				} else {
					#print "Couldn't parse: ", Dumper([$orig,$newname,$name]);
					last;
				}
			}
			$name = $newname;
			#length $name or print Dumper [name=>join '', map /\w/?$_:sprintf("%%%02x",ord), split //, join '^', $name,$orig];
		}
		my ($perm, $type, $sha1, @size) = split ' ', $ptss;
		my $size = pop @size;
		$size =~ /\D/ and $size = 0;
		$size = 0 + $size;
		$name =~ s{^\Q$dir\E}{};
		push @got, { perm => oct "0$perm", type => $type, sha1 => $sha1, size => $size, name => $name, branch => $branch, fn => $orig_fn };
	}
	@got;
}
my %cached_times;
sub _sha1_commit_time {
	my $rev = shift;
	if (!$cached_times{$rev}) {
		my ($commit) = $git->command('log', '--pretty=format:%ct', $rev);
		$cached_times{$rev} = 0 + $commit;
	}
	$cached_times{$rev};
}
sub _getbranchtime {
	my $info = shift;
	my @times;
	if ($$info{branch}) {
		if (my $rev = _rev $$info{branch}) {
			@times = (_sha1_commit_time $rev);
		}
	}
	@times = (@times) x 3 if @times == 1;
	@times = (time, time, time) unless @times;
	@times;
}
=begin STAT fields
00 dev      device number of filesystem
01 ino      inode number
02 mode     file mode  (type and permissions)
03 nlink    number of (hard) links to the file
04 uid      numeric user ID of file's owner
05 gid      numeric group ID of file's owner
06 rdev     the device identifier (special files only)
07 size     total size of file, in bytes
08 atime    last access time in seconds since the epoch
09 mtime    last modify time in seconds since the epoch
10 ctime    inode change time in seconds since the epoch (*)
11 blksize  preferred block size for file system I/O
12 blocks   actual number of blocks allocated
=cut
sub _fakestat {
	my ($info) = @_;
	my $perm = $$info{perm};
	$perm |= 0444 unless $perm & 0444;
	my $size = $$info{size} // 0;
	my $inode = 0; # TODO
	my $links = 1; # TODO
	if ($perm & 040000) {
		$perm |= 0111;
		$size ||= 4096;
		$links++;
		$links++; # fool 'find' (which optimizes link=2 case)
	} elsif (($perm & 0120000) == 0120000) {
		$perm |= 0777;
	}
	my @times;
	my @intimes = qw/atime mtime ctime/;
	if ($$info{times} or grep defined, @$info{@intimes}) {
		@times = ($$info{times}) x 3;
		my $now;
		$times[$_] //= $$info{$intimes[$_]} // ($now //= time) for 0..$#times;
	} elsif ($::notime) {
		@times = (time) x 3;
	} else {
		@times = _getbranchtime $info;
	}
	1234, $inode, $perm, $links, 0+$<, 0+$(, 0, $size, @times, 4096, 0;
}
sub _actual {
	my ($rev, $dir) = @_;
	return unless $rev eq '.working';
	join '/', $git->wc_path, $dir;
}
our @date_defaults;
BEGIN { @date_defaults = (0, 1, 1, 0, 0, 0); }
sub getattr {
	&_de;
	my $fn = shift;
	my @ret = (-&ENOENT);
	if ($fn eq '/') {
		@ret = _fakestat {perm => 16384}
	} elsif ($fn =~ m{^/\.working\b}) {
		my ($dir) = (_parts $fn)[-1];
		if (my $actual = _actual '.working', $dir) {
			@ret = stat $actual;
		}
	} elsif ($fn =~ m{^/\.bydate((?:/\d+){1,6})$}) {
		my @time = split '/', substr $1, 1;
		$time[0] -= 1900 if @time;
		$time[1] -= 1 if @time > 1;
		push @time, @date_defaults[@time..$#date_defaults];
		@time = reverse @time;
		my $time = timelocal @time;
		@ret = _fakestat {perm => 16384, times => $time};
	} elsif ($fn =~ m{^/([^/]+)$}) {
		my $rev = $1;
		@ret = _fakestat {perm => 16384} if _rev $rev;
	} else {
		if (my ($info) = _lsfile $fn) {
			@ret = _fakestat $info;
			if ($::debug > 1) {
				print Dumper [$fn, $info, \@ret];
			}
		}
	}
	@ret;
}
sub getdir {
	&_de;
	my $fn = shift;
	my @files = (qw/. ../);
	if ($fn eq '/') {
		push @files, HEAD => (keys %special), _branches;
	} elsif ($fn =~ m{^/\.working\b}) {
		my ($dir) = (_parts $fn)[2];
		opendir my $d, $git->wc_path.'/'.$dir or return -$!;
		@files = readdir $d;
		close $d;
	} elsif ($fn =~ m{^/\.bydate((?:/\d+){0,6})$}) {
		my (@YmdHMS) = split '/', substr $1, 1;
		my @time_args;
		if (@YmdHMS) {
			my @since = (@YmdHMS, @date_defaults[@YmdHMS..$#date_defaults]);
			my @until = (@YmdHMS, @date_defaults[@YmdHMS..$#date_defaults]);
			$until[$#YmdHMS]++;
			push @time_args, map sprintf('--%s=%04d-%02d-%02d %02d:%02d:%02d', @$_),
				[ after => @since ],
				[ before => @until ];
		}
		if (my @revs = $git->command('rev-list', '--all', @time_args)) {
			my ($beg, $end) = map [(localtime $_)[reverse 0..5]], map _sha1_commit_time($_), @revs[-1,0];
			$$_[0] += 1900 for $beg, $end;
			$$_[1] += 1 for $beg, $end;
			@files = map sprintf("%02d", $_), ($$beg[@YmdHMS]..$$end[@YmdHMS]);
		}
	} else {
		push @files, map $$_{name}, _lsfile "$fn/";
		$::debug and print Dumper [_lsfile "$fn/"], [@files];
	}
	@files, 0;
}
sub unlink { &_de; $_[0] eq '/unmount' and exit; -&EOPNOTSUPP }
sub readlink {
	&_de;
	my ($fn, $branch, $dir) = _parts shift;
	if (my $actual = _actual $branch, $dir) {
		return readlink $actual;
	}
	my ($info) = _lsfile $fn;
	$info or return '';
	$$info{perm} & 0120000 or return '';
	$git->command('cat-file', '-p', $$info{sha1});
}
sub open { 0 }
sub flush { 0 }
sub release { 0 }
sub read {
	&_de;
	my ($fn, $commitish, $dir) = _parts shift;
	my ($len, $off) = @_;
	my $rev = _rev $commitish;
	$::debug > 1 and print Dumper {
		fn => $fn,
		commitish => $commitish,
		dir => $dir,
		rev => $rev,
		special => $special{$commitish},
	};
	$rev or return -&EPERM;
	if (my $actualfile = _actual $rev, $dir) {
		$::debug > 1 and print "Trying actual file: $actualfile\n";
		(-f $actualfile) or return -&ENOENT;
		$::debug and print "Using actual file: $actualfile\n";
		CORE::open my $f, '<', $actualfile or return -$!;
		seek $f, $off, 0;
		local $/ = \$len;
		my $ret = <$f>;
		close $f;
		return $ret;
	}
	$::debug and print "Reading from git $commitish(=$rev)\n";
	my ($info) = _lsfile $fn;
	$info or return -&ENOTFOUND;
	$$info{type} eq 'blob' or return -&EOPNOTSUPP;
	my $sha1 = $$info{sha1};
=begin more efficient way?
	my $gitdir = $git->repo_dir;
	my $objfile = join '/', $gitdir, substr($sha1, 0, 2), substr($sha1, 2);
	if (-f $objfile) {
		open my $f, '<', $objfile;
=cut
=begin fuck - doesn't handle packs
	my $storage = "\0" x $$info{size};
	CORE::open my $temp, '+<', \$storage or return -&EOPNOTSUPP;
	$git->cat_blob($sha1, $temp);
	seek $temp, $off, 0;
	local $/ = \$len;
	my $ret = <$temp>;
	close $temp;
	$ret
=cut
	my $file = $git->command('cat-file','-p',$sha1);
	substr $file, $off, $len;
}
our $AUTOLOAD;
sub AUTOLOAD {
	(my $_sub = $AUTOLOAD) =~ s{^.*::}{};
	eval "sub $_sub { &_de; -&EOPNOTSUPP }";
	goto &$_sub;
}
