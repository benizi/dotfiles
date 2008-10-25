#!/usr/bin/perl
use strict;
use warnings;
use File::Basename;
use File::Find;
use feature ':5.10';
my @port_dirs = my ($installed, $portdir, $local, $retired) = qw{
	/var/db/pkg
	/usr/portage
	/var/paludis/repositories/local
	/var/paludis/repositories/retired
};
shift @port_dirs;

use File::Spec;
use Getopt::Long qw/:config pass_through/;
GetOptions(
	'package=s' => \(my $PACK = ''),
	'dry' => \(my $dry = 0),
	'verbose+' => \(my $verbose = 0),
	'user|pu=s' => \(my $pu = 'paludisbuild'),
	'group|pg=s' => \(my $pg = 'paludisbuild'),
	'cache!' => \(my $cache_too = 1),
) or die 'options';
$dry and $verbose ||= 1;
umask 0022;

sub error { say "\e[31m", @_, "\e[0m" }
sub mkdir_s {
	state %s;
	my $_ = shift;
	$s{$_}++ and return;
	$verbose and say "mkdir $_";
	$dry or mkdir;
}
sub mkdir_p {
	my $_ = shift;
	my @p = File::Spec->splitdir($_);
	mkdir_s $_ for grep !-d, map File::Spec->catdir(@p[0..$_]), 0..$#p-1;
}
sub copy {
	my ($src, $dst) = @_;
	mkdir_p $dst;
	$verbose and say for "cp", $src, dirname $dst;
	$dry and return;
	open my $if, '<', $src or die "<$src: $!";
	open my $of, '>', $dst or die "<$dst: $!";
	print $of $_ for <$if>;
	close $if;
	close $of;
}
sub choose {
	my $choice = @_ != 1 ? 0 : 1;
	while (!$choice) {
		say "[", $_+1, "] $_[$_]" for 0..$#_;
		say "[0] cancel";
		say "Choice [", 0+@_, "]?";
		chomp($choice = <>);
		given ($choice) {
			when (0) { error "canceled"; exit }
			when (/^\d+$/ and $_ <= @_) { last }
			when (!length) { $choice = @_; last }
			default { say "Bad choice"; $choice = 0 }
		}
	}
	$choice ||= 1;
	say "Chose [$choice] $_[$choice-1]";
	$_[$choice-1];
}

my $version_re = qr/((?:cvs\.)?)(\d+)((?:\.\d+)*)([a-z]?)((?:_(?:pre|p|beta|alpha|rc)\d*)*)((?:-r((?:\d+)?))?)$/;
sub get_package {
	my ($pkg, $versioned) = @_;
	(my $pkge = $pkg) =~ s/([\$\'\`\s])/\\$1/g;
	my @pkgs;
	for my $opt ('-e', '-r') {
		chomp(@pkgs = readpipe "eix --only-names $opt $pkge");
		last if @pkgs;
	}
	@pkgs = ($pkg) if !@pkgs and $pkg =~ m{/};
	if (!@pkgs) {
		my ($cat, $pack) = split m{/}, $pkg;
		($cat, $pack) = $pack ? ("\Q$cat\E", $pack) : ('.', $cat);
		if ($versioned) {
			find {
				wanted => sub {
					return if !-f;
					return if !/\.ebuild$/;
					(my $dir = $File::Find::dir) =~ s{^\Q$installed\E/}{};
					die "No version in $File::Find::dir??\n" unless (my $ver = $dir) =~ s/-$version_re//;
					push @pkgs, $dir;
				},
				preprocess => sub {
					if ($File::Find::dir eq $installed) {
						return grep /$cat/, @_;
					}
					grep /^$pack/, @_;
				},
			}, $installed;
		}
		!@pkgs and error "Nothing matched {$pkg}" and return;
	}
	choose @pkgs
}

sub get_ebuild {
	my ($src, $pkg, $versioned) = @_;
	my @dirs = glob File::Spec->catfile($src, $pkg);
	my @ebuilds = map glob(File::Spec->catfile($_,'*.ebuild')), @dirs;
	!@ebuilds and error join("\n", "No ebuilds for {$pkg}", map "Looked in $_", (@dirs?@dirs:('nowhere?!'))) and return;
	choose @ebuilds
}

sub interactive_move {
	my ($src, $dst, $versioned, @files) = @_;
	my $ebuild = $files[0];
	my $pacv = File::Spec->catfile(
		(basename dirname dirname $ebuild),
		(basename dirname $ebuild),
	);
	my $pacn = $pacv;
	for ($pacn) { 1 while s/-r\d+$// or s/-\d[^-]*$//; }

	my $allchoice;
	for (@files) {
		my $skip;
		my $dfn = $_;
		for ($dfn) {
			s{\Q$src\E}{$dst};
			s{\Q$pacv\E}{$pacn};
		}
		while (-f $dfn) {
			say "Overwrite $dfn [y/*y/N/*n/x=exit]?";
			given ($allchoice // <>) {
				when (/^(\*?)y/i) { $1 and $allchoice = $_; $skip = 0 }
				when (/^(\*?)n/i) { $1 and $allchoice = $_; $skip = 1 }
				when (/^x/i or /^exit/i) { error "Aborting"; exit }
				default { say "Bad choice" }
			}
			last if defined $skip;
		}
		$skip and error "Skipping $_" and next;
		copy $_, $dfn;
	}
}
sub portage_permissions {
	my @pdirs = @_ ? @_ : ($portdir, grep -d, glob "/usr/local/*portage");
	my $am_root = ($< == 0) ? 1 : 0;
	my $paludis_u = 0 + getpwnam $pu;
	my $paludis_g = 0 + getgrnam $pg;
	my @tochange;
	find {
		wanted => sub {
			my ($perm) =
				(-f) ? 0664 :
				(-d) ? 0775 :
				die "!-f, !-d: $File::Find::name\n";
			$verbose > 1 and $am_root and say "root: chown/chmod $File::Find::name";
			$dry and return;
			push @tochange, $File::Find::name and return if !$am_root;
			chown $paludis_u, $paludis_g, $_;
			chmod $perm, $_;
		},
		preprocess => sub {
			grep !/^\.cache$/ || $cache_too,
			@_
		},
	}, @pdirs;
	if (!$am_root and @tochange) {
		my @xargs = ("sudo", "xargs", "-0", "--no-run-if-empty");
		$dry or open my $chown, "| @xargs chown $pu:$pg" or die "| chown: $!";
		$dry or open my $chmod, "| @xargs chmod a+rX,g+w" or die "| chown: $!";
		for (@tochange) {
			$verbose > 1 and say "non-root: chown/chmod $_";
			$dry and next;
			print $chown "$_\0";
			print $chmod "$_\0";
		}
	}
}
sub ebuild_mover {
	my ($src, $dst, $versioned, $pack) = @_;
	return unless $pack = get_package $pack, $versioned;
	return unless my $ebuild = get_ebuild $src, $pack, $versioned;
	interactive_move $src, $dst, $versioned, $ebuild,
		grep -f, glob File::Spec->catfile(dirname($ebuild), 'files', '*');
	portage_permissions $dst
}
my @args = (grep($_, $PACK), @ARGV);
@ARGV = ();
given (lc basename $0) {
	when ('unmask') {
		say 'unmask'
	} when ('ebuild-get-package') {
		@args = ($portdir, @args) if @args < 3;
		get_package @args;
	} when ('ebuild-to-local') {
		ebuild_mover $portdir, $local, 0, @args;
	} when ('ebuild-retire') {
		ebuild_mover $installed, $retired, 1, @args;
	} when (/perm/i) {
		$verbose++ if $verbose;
		portage_permissions @args;
		$verbose-- if $verbose;
	} default {
		say;
	}
}
__END__
function unmask () {
	EPPK=/etc/portage/package.keywords
	if grep "$1" $EPPK
	then echo Already unmasked
	else echo "$1" >> $EPPK
	fi
}
function ebuild-get-package () {
	DIR=$1
	PACK=$2
	VERSIONED=$3
	if $VERSIONED ; then XTRA='[^a-z0-9][0-9]*' ; else XTRA='' ; fi
	MULT=false
	case $PACK in
		$DIR/*) ;;
		*/*) PACK=($DIR/$PACK${~XTRA}) ; MULT=true ;;
		*) PACK=($DIR/*/$PACK${~XTRA}) ; MULT=true ;;
	esac
	if $MULT && ! $VERSIONED ; then
		[ $#PACK != 1 ] && print -l "Multiple matches:" $PACK && return
	fi
	if $VERSIONED ; then
		EBUILDS=()
		for p in $PACK ; echo $p && EBUILDS=($EBUILDS $p/$p:t.ebuild)
	else
		EBUILDS=($PACK/$PACK:t-*.ebuild)
	fi
	if [ $#EBUILDS = 1 ] ; then
		echo only choice - $EBUILDS
	fi
	while [ $#EBUILDS != 1 ] ; do
		for i in {1..$#EBUILDS} ; echo $i: $EBUILDS[i]
		echo 0: cancel
		read "choice?Choice [$#EBUILDS]: "
		for l in "$choice" ; case "$l" in # for-case so it can 'break'
			'') choice=$#EBUILDS ; echo null ;;
			0) error "cancelled" ; return 1 ;;
			<->) [ $choice -le $#EBUILDS ] && break ;&
			*) error "Should be a number from 0-$#EBUILDS" ; continue 2
		esac
		EBUILDS=($EBUILDS[choice])
		echo CHOSE $choice - $EBUILDS
	done
	return
}
function interactive-move () {
}
function ebuild-to-local () { ebuild-mover /usr{,/local}/portage false $argv }
