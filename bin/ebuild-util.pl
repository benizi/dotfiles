#!/usr/bin/perl
use strict;
use warnings;
use File::Basename;
use feature ':5.10';
my @port_dirs = my ($pkg, $portdir, $local, $retired) = qw{
	/var/db/pkg
	/usr/portage
	/usr/local/portage
	/usr/local/retired-portage
};
shift @port_dirs;

use File::Spec;
use Getopt::Long qw/:config pass_through/;
GetOptions(
	'package=s' => \(my $PACK = ''),
	'dry' => \(my $dry = 0),
	'verbose+' => \(my $verbose = 0),
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

sub get_package {
	my ($pkg, $src) = @_;
	(my $pkge = $pkg) =~ s/([\$\'\`\s])/\\$1/g;
	my @pkgs;
	for my $opt ('-e', '-r') {
		chomp(@pkgs = readpipe "eix --only-names $opt $pkge");
		last if @pkgs;
	}
	@pkgs = ($pkg) if !@pkgs and $pkg =~ m{/};
	!@pkgs and error "Nothing matched {$pkg}" and return;
	choose @pkgs
}

sub get_ebuild {
	my ($src, $pkg, $versioned) = @_;
	my @dirs = glob File::Spec->catfile(
		$src,
		$pkg . ($versioned ? '-[0-9]*' : '')
	);
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
	for (grep basename($_) ne '.cache',
		grep -d,
		map glob(File::Spec->catfile($_,'*')),
		$local, $retired) {
		my @cmd = ('chmod', '-R', 'a+rX', $_);
		$verbose and say "@cmd";
		$dry or system { $cmd[0] } @cmd;
	}
}
sub ebuild_mover {
	my ($src, $dst, $versioned, $pack) = @_;
	return unless $pack = get_package $pack;
	return unless my $ebuild = get_ebuild $src, $pack, $versioned;
	interactive_move $src, $dst, $versioned, $ebuild,
		grep -f, glob File::Spec->catfile(dirname($ebuild), 'files', '*');
	portage_permissions
}
my @args = (grep($_, $PACK), @ARGV);
@ARGV = ();
given (lc basename $0) {
	when ('unmask') {
		say 'unmask'
	} when ('ebuild-get-package') {
		@args = ($portdir, @args) if @args < 3;
		get_package @args
	} when ('ebuild-to-local') {
		ebuild_mover $portdir, $local, 0, @args
	} when ('ebuild-retire') {
		ebuild_mover $pkg, $retired, 1, @args;
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
