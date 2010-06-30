#!/usr/bin/perl
use strict;
use warnings;
use bytes;
use Getopt::Long;
GetOptions(
	'mountpoint=s' => \$::mount_point,
	'debug+' => \($::debug = 0),
	'branchslash=s' => \($::slash = '%'),
	'gitdir=s' => \$::gitdir,
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
defined $::mount_point or die "Need to specify --mountpoint\n";
defined $::gitdir or die "Need to specify --gitdir\n";
use Fuse;
package Fuse;
use Git;
use Data::Dumper;
use IPC::Run 'run';
sub umount { run [fusermount=>'-u'=>$::mount_point] }
$SIG{INT} = sub { exit };
$SIG{HUP} = sub { exit };
print "kill -HUP $$\n";
END { umount }
umount;
my $git = Git->repository($::gitdir);
main(mountpoint=>$::mount_point,map {; $_, $_ }
qw/getattr getdir readlink mknod mkdir unlink rmdir symlink
rename link chmod chown truncate utime open read
write statfs flush release fsync setxattr getxattr
listxattr removexattr/);
use POSIX qw/:errno_h ENOENT/;
sub _de { $::debug or return; warn "", (caller(1))[3], "(@_)\n"; }
sub _branches { my @br = $git->command('branch'); s{^..}{} and s{/}{$::slash}g for @br; @br }
sub _parts {
	my $fn = shift;
	my @parts = split m{/}, $fn, -1;
	shift @parts;
	my $branch = shift @parts;
	my $dir = join '/', @parts;
	($fn, $branch, $dir)
}
sub _lsfile {
	my ($fn, $branch, $dir) = _parts shift;
	my @got;
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
		push @got, { perm => oct "0$perm", type => $type, sha1 => $sha1, size => $size, name => $name };
	}
	@got;
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
	my $links = 1; # TODO
	if ($perm & 040000) {
		$perm |= 0111;
		$size ||= 4096;
		$links++;
	} elsif (($perm & 0120000) == 0120000) {
		$perm |= 0777;
	}
	1234, 1234, $perm, $links, 0+$<, 0+$(, 0, $size, time, time, time, 4096, 0;
}
my %special = map {; $_ => 1 } qw/.index .working/;
sub _rev {
	my ($rev, $subst) = @_;
	$subst and $rev =~ s/$::slash/\//g;
	return $rev if $special{$rev};
	my @out;
	eval {
		@out = $git->command(['rev-parse','-q','--verify',$rev],{STDERR=>0});
	};
	return _rev($rev, 1) if !@out and $rev =~ /$::slash/;
	return unless @out;
	shift @out;
}
sub getattr {
	&_de;
	my $fn = shift;
	my @ret = (-&ENOENT);
	if ($fn eq '/') {
		@ret = _fakestat {perm => 16384}
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
	my ($info) = _lsfile $fn;
	$info or return '';
	$$info{perm} & 0120000 or return '';
	$git->command('cat-file', '-p', $$info{sha1});
}
our $AUTOLOAD;
sub AUTOLOAD {
	(my $_sub = $AUTOLOAD) =~ s{^.*::}{};
	eval "sub $_sub { &_de; -&EOPNOTSUPP }";
	goto &$_sub;
}
