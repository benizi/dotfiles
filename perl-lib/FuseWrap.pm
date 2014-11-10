package FuseWrap;
use strict;
use warnings;
use base 'Fuse';
use Carp 'verbose';
use FindBin '$Bin';
use IPC::Run 'run';
use Getopt::Long qw/:config pass_through/;
our ($mountpoint, $debug, $allusers, $just_umount);
BEGIN {
	GetOptions(
		'mountpoint=s' => \$mountpoint,
		'debug+' => \($debug = 0),
		'allusers!' => \($allusers = 1),
		'umount' => \($just_umount),
	);
	no warnings 'redefine';
	sub GetOptions (@) {
		my $ret = &Getopt::Long::GetOptions;
		$mountpoint //= shift @ARGV if @ARGV;
		$ret;
	}
}
use Fcntl ':mode';
use POSIX qw/ENOENT EOPNOTSUPP/;
our (%default_stat, @stat, @fields);
BEGIN {
	%default_stat = (
		qw/dev 1 ino 1 mode 0 nlink 1/ =>
		uid => 0+$< => gid => 0+$( =>
		qw/rdev 0 size 4096/ =>
		map(($_ => time), qw/atime mtime ctime/) =>
		qw/blksize 4096 blocks 4096/,
		readlink => '',
	);
	@stat = qw/dev ino mode nlink uid gid rdev size atime mtime ctime blksize blocks/;
	@fields = (readlink => @stat);
}
sub _fake_attr { my ($name, %stat) = (shift, %default_stat, @_); @stat{@stat} }
sub _fake_attr_file { _fake_attr @_, mode => 0100000 | 0644 }
sub _fake_attr_dir { _fake_attr @_, nlink => 3, mode => 0040000 | 0755 }
sub _fake_attr_ {
	my $base = (split '/', $_[0])[-1];
	if (length($base) and $base eq lc $base) {
		&_fake_attr_file;
	} else {
		&_fake_attr_dir;
	}
}
our $mounted;
sub _de {
	my ($name, $sub) = splice @_, 0, 2;
	my @p = $debug ? @_ : ();
	my @ret = &$sub;
	if ($debug) {
		use Data::Dumper;
		print Dumper [ $name, $sub, \@p, \@ret ];
	}
	@ret;
}

my @funcs;
BEGIN {
	@funcs = qw/
		getattr getdir readlink mknod mkdir unlink rmdir symlink
		rename link chmod chown truncate utime open read
		write statfs flush release fsync
		setxattr getxattr listxattr removexattr
	/;
}
sub umount { my @cmd = (fusermount => -u => $mountpoint); print "@cmd\n"; run \@cmd }
sub mounted { local @ARGV = ('/etc/mtab'); grep m{^\S+\s+\Q$mountpoint\E\s}, <> }
END { umount if mounted }
if ($just_umount) {
	umount if mounted;
	exit;
}
sub main {
	my $sym = \%::;
	$sym = $$sym{$_.'::'} for split /::/, __PACKAGE__;
	my %un;
	for my $fn (@funcs) {
		my $sub = (exists $$sym{$fn}) ? \&$fn
			: sub { $un{$fn}++ or warn "unimplemented($fn)\n"; -&EOPNOTSUPP };
		no strict 'refs';
		no warnings 'redefine';
		*$fn = sub { _de $fn, $sub, @_ };
	}
	$mountpoint // die "Must specify mount point\n";
	umount if mounted;
	Fuse::main(
		mountpoint => $mountpoint,
		grep($debug > 1, qw/debug 1/),
		grep($allusers, qw/mountopts allow_other/),
		map {; $_ => 'FuseWrap::'.$_ } @funcs
	);
	umount if mounted;
}
1;
