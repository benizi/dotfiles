#!/usr/bin/perl
use strict;
use warnings;
use List::Util 'max';
use File::Spec;
use File::Basename;
use IPC::Run 'run';
use Cwd;

sub _color { "\e[$_[0]m$_[1]\e[0m" }
sub red ($) { _color 31, $_[0] }
sub green ($) { _color 32, $_[0] }
sub blue ($) { _color 34, $_[0] }
sub varval { my ($var, $val) = @_; red($var).blue("=")."<".green($val).">" }
my $logfile;
sub lprint { print @_; $logfile and print $logfile @_ }
sub info { lprint ":"; lprint " ", varval splice @_, 0, 2 while @_; lprint "\n"}
sub status { lprint blue "@_"; lprint "\n" }
sub mydie { lprint red "@_"; lprint "\n"; exit 1 }

use Getopt::Long qw/:config pass_through/;
my ($ctarget, $cross_cat, $ctarget_implicit);
my @options_help = (
	't|target=s' => \$ctarget, 'TARGET', 'Specify the target \'tuple\'',
	'with-headers' => \(my $do_headers), '', 'Build just the headers first',
	'bv=s' => \(my $bv), 'VERSION', 'Specify version for binutils',
	'gv=s' => \(my $gv), 'VERSION', 'Specify version for gcc',
	'kv=s' => \(my $kv), 'VERSION', 'Specify version for kernel',
	'lv=s' => \(my $lv), 'VERSION', 'Specify version for libc',
	'dry|dry-run' => \(my $dry), '', 'Just print, don\'t do.',
	'use=s@' => \my @use, 'FLAG', 'Extra USE flags',
	'extra-gcc' => \(my $extra_gcc), '', 'Extra gcc stuff',
	'extra-gdb' => \(my $extra_gdb), '', 'Install gdb',
	'conf-dir=s' => \(my $config_dir = '/etc/paludis'), 'DIR',
		'Where paludis configuration resides',
	'source-repo=s' => \(my $source_repo = 'gentoo'), 'REPO[,REPO]',
		'Master repository (source of packages)',
	'cross-repo=s' => \(my $cross_repo = 'crossdev'), 'REPO',
		'Repo for cross-packages',
	'log-dir=s' => \(my $cdev_logs = cwd), 'DIR',
		"Store ".(basename $0)." logs in this dir",
	'add-to-world' => \(my $world = 0), '', 'Add packages to \'world\' set',
	'help|?' => \(my $do_help = 0), '', 'Show usage information',
	'_recursive' => \(my $_recursive = 0), 'INTERNAL', '',
	'<>' => sub {
		die usage("CTARGET defined more than once") if defined $ctarget;
		$ctarget = shift;
		$ctarget_implicit = $ctarget;
	}, 'INTERNAL', '',
);
sub dryrun {
	my @cmd = @{$_[0]};
	info RUNNING => "@cmd";
	$dry and return;
	&run or mydie "Error running @cmd";
}
my (@options);
my (@flag, @desc);
while (my ($getopt, $ref, $arg, $desc) = splice @options_help, 0, 4) {
	push @options, $getopt, $ref;
	next if $arg eq 'INTERNAL';
	my $has_arg = (my $flag = $getopt) =~ s/=.*$//;
	$flag = join '/', map /^(.)$/ ? "-$_" : "--$_", split /\|/, $flag;
	$flag .= " $arg" if $has_arg;
	push @flag, $flag;
	push @desc, $desc;
}
my $l = max map length, @flag;
my @usage = map sprintf("%-*s    %s", $l, $flag[$_], $desc[$_]), 0..$#flag;
sub usage {
	@_ = map "\e[31mERROR => $_\e[0m", @_;
	join '', map $_.$/, @_, "Usage: $0 [options] [TARGET]", @usage, @_
}
my @original_ARGV = @ARGV;
GetOptions(@options) or die usage;
$do_help and die usage;
$ctarget = shift if @ARGV and not defined $ctarget;
@ARGV and die usage "Extra arguments: @ARGV";

sub recursively_crossdev {
	my @perl = ($^X, $0);
	mydie "Recursive loop (called as @perl @original_ARGV)" if $_recursive;
	my @recursive_args = grep {
		!$ctarget_implicit or !($_ eq $ctarget_implicit)
	} @original_ARGV;
	dryrun [ @perl, @recursive_args, '--_recursive', $_ ] for @_;
	exit;
}

my ($portage_arch);
my @binutils = qw{sys-devel binutils};
my @gcc = qw{sys-devel gcc};
my @kernel = qw{sys-kernel linux-headers};
my @libs = qw{sys-libs};
my @gdb = qw{sys-devel gdb};
my @insight = qw{dev-util insight};

my @stages = qw/binutils c kernel libc cpp/;

my @guse = ();
my @guse_disable = qw/-boundschecking -d -fortran -gtk -gcj -libffi -mudflap nocxx -objc -objc++ -objc-gc -openmp/;
my @guse_disable_stage2 = grep !(/-fortran/||/nocxx/), @guse_disable;
my $max = $stages[-1];
my $def_headers = 1;

sub setup_use_dir {
	dryrun [ 'mkdir', '-p', File::Spec->catfile($config_dir, 'use.conf.d') ];
}

use Memoize;
memoize 'repo_dir';
sub repo_dir {
	my $repo_name = shift;
	run [ paludis => '--configuration-variable', $repo_name, 'location' ],
		'>', \my $dir, '2>', \my $errors;
	mydie $errors if $errors;
	chomp($dir);
	$dir;
}

sub repo_pack_dir {
	my ($repo, $cat, $pack) = @_;
	File::Spec->catfile(repo_dir($repo), $cat, $pack);
}
sub create_cross_pack {
	my ($cat, $pack) = @_;
	my $out_dir = repo_pack_dir $cross_repo, $cross_cat, $pack;
	info OUT_DIR => $out_dir;
	my @search = map repo_pack_dir($_, $cat, $pack), split /,/, $source_repo;
	my $err = join "\n",
		"No available source for $cat/$pack in {$source_repo}. Tried:",
		@search;
	my ($src_dir) = grep -d, @search;
	mydie $err unless $src_dir;
	return if -l $out_dir;
	mydie "$src_dir not a symbolic link" if -e $out_dir;
	info SRC_DIR => $src_dir;
	my $out_parent = dirname $out_dir;
	dryrun [ mkdir => -p => $out_parent ] unless -d $out_parent;
	dryrun [ ln => -s => $src_dir => $out_dir ];
}

sub P { my ($c, $p, $v) = @_; defined($v) ? "=$c/$p-$v" : "$c/$p" }

use File::Temp 'tempfile';
sub set_pkg_use {
	my ($cat, $pack, $use, $cross, $version) = @_;
	my $P = P $cat, $pack, $version;
	my $generic = "$cat/$pack";
	my $use_line = join " ", grep length, grep defined,
		$P, @$use, 'CROSSCOMPILE_OPTS:', @$cross;
	info SETTING_USE => $use_line;
	my ($tmphandle, $tmpnam) = tempfile;
	my $use_conf = "$config_dir/use.conf.d";
	my $conf = "$use_conf/auto.$cross_repo.conf";
	my @old;
	chomp(@old = do { local @ARGV = $conf; <> }) if -f $conf;
	@old = grep index($_, $P), grep index($_, $generic), @old;
	print $tmphandle "$_\n" for @old, $use_line;
	dryrun [ mkdir => -p => $use_conf ];
	dryrun [ mv => $tmpnam, $conf ];
	unlink $tmpnam if -f $tmpnam;
}

sub install {
	my ($label, $cat_pack, $version, %opts) = @_;
	info NOTHING_FOR => $label and return if !@$cat_pack;
	mydie "CATPACK=[@$cat_pack]\n" unless 2 == @$cat_pack;
	my ($cat, $pack) = @$cat_pack;
	my @cross_opts = map split, grep defined, $opts{CC};
	my @this_use = map @$_, grep defined, $opts{USE};
	my $upgrade_if_changed = ($opts{IF_USE_CHANGED} ? 1 : 0);
	my $upgrade = ($opts{UPGRADE} ? 1 : 0);
	print "Install $cat/$pack",($version?"-$version":"")," ($label)\n";
	info CROSSCOMPILE_OPTS => "@cross_opts",
		USE => "@this_use",
		UPGRADE => ($upgrade ? 'yes' : $upgrade_if_changed ? 'if-changed' : 'no'),
		;
	my %used = map {; $_ => 1 } qw/CC USE UPGRADE IF_USE_CHANGED/;
	my @unused = grep !$used{$_}, keys %opts;
	info UNUSED => join ", ", @unused if @unused;
	setup_use_dir;
	create_cross_pack $cat, $pack;
	set_pkg_use $cross_cat, $pack, [@this_use], [@cross_opts], $version;
	dryrun [ paludis =>
		$upgrade ? (qw/--dl-reinstall always --dl-upgrade always/)
		: $upgrade_if_changed ? (qw/--dl-reinstall if-use-changed/)
		: (),
		'--dl-deps-default', 'discard',
		grep(!$world, '-1'),
		'--install', P $cross_cat, $pack, $version
	];
	status "Success: $label";
}

sub setup_ctarget {
	die usage "Need to specify TARGET" unless defined $ctarget;

	if ($ctarget !~ /-/) {
		for ($ctarget) {
			if (/^amd64$/) { $_ = 'x86_64' }
			elsif (/^parisc/) { s/parisc/hppa/ }
			elsif (/^ppc(?:64)?$/) { s/ppc/powerpc/ }
			elsif (/^x86$/) { $_ = 'i686' }
		}

		for ($ctarget) {
			if (/i.86/ or /x86_64/) { $_ .= '-pc-linux-gnu' }
			elsif (/^s390/) { $_ .= '-ibm-linux-gnu' }
			elsif (/^(?:alpha|arm|cris|hppa|ia64|m68|mips|powerpc|sparc|sh)/) {
				$_ .= '-unknown-linux-gnu' }
			elsif (/^(?:bfin|h8300|nios2|spu|xc16x)/) { $_ .= '-elf' }
		}
	}

	for ($ctarget) {
		if (my @arch = grep $ctarget =~ /^\Q$_\E/,
			qw{alpha arm hppa ia64 mips sparc s390 sh}) {
			($portage_arch) = @arch }
		elsif (/^i?86/) { $portage_arch = 'x86' }
		elsif (/^m68/) { $portage_arch = 'm68k' }
		elsif (/^powerpc64/) { $portage_arch = 'ppc64' }
		elsif (/^powerpc/) { $portage_arch = 'ppc' }
		elsif (/^x86_64/) { $portage_arch = 'amd64' }
		else { $portage_arch = /^([^\-]+)/ ? $1 : $_ }
	}

	if ($portage_arch eq 'ia64') { $def_headers = 1 }

	for ($ctarget) {
		if (/-freebsd/) { $_ .= '-fbsd' }
	}

	for ($ctarget) {
		if (/^avr/) { splice @kernel; $max = 'libc'; @libs = qw{dev-embedded avr-libc}; @guse = @guse_disable; $def_headers = 0 }
		elsif (/-cygwin/) { @guse_disable = grep !/nocxx/, @guse_disable }
		elsif (/^mingw/ or /-mingw/) { $def_headers = 1; @kernel = qw{dev-util w32api}; @libs = qw{dev-util mingw-runtime} }
		elsif (/^msp430$/) { $max = 'binutils' }
		elsif (/^nios2$/) { $$_[-1] .= '-nios2' for \@binutils, \@gcc }
		elsif (/^cell$/) { recursively_crossdev qw/ppu spu-elf/ }
		elsif (/^spu/) { $portage_arch = 'ppc64'; splice @kernel; push @libs, 'newlib' }
		elsif (/^ppu/) { $portage_arch = 'ppc64' }
		elsif (/^ps2$/) { recursively_crossdev qw/ee iop dvp/ }
		elsif (/^ee/) { $portage_arch = 'mips'; splice @kernel; $bv = '2.14'; $gv = '3.2.2'; $max = 'c'; @guse = @guse_disable }
		elsif (/^iop/) { $portage_arch = 'mips'; $bv = '2.14'; $gv = '3.2.2'; $max = 'c'; @guse = @guse_disable }
		elsif (/^dvp/) { $portage_arch = 'mips'; $bv = '2.14'; $gv = '3.2.2'; $max = 'binutils' }
	}

	for ($ctarget) {
		if (/-dietlibc/) { @libs = qw{dev-libs dietlibc} }
		elsif (/-gnu/) { push @libs, 'glibc' }
		elsif (/-klibc/) { push @libs, 'klibc' }
		elsif (/-uclibc/ or /-uclinux/) { push @libs, 'uclibc' }
		elsif (/-cygwin/) { @libs = qw{dev-libs cygwin}; splice @kernel }
		elsif (/-newlib/ or /-elf/) { push @libs, 'newlib'; splice @kernel; $max = 'c' }
		elsif (/-freebsd/) { @libs = qw{sys-freebsd freebsd-lib}; splice @kernel }
	}

	pop @libs if @libs == 1;
	info CTARGET => $ctarget;
	$cross_cat = "cross-$ctarget";
}

sub do_stages {
	$do_headers = $def_headers unless defined $do_headers;
	my $preinstall = 0;
	for my $stage (@stages) {
		print "Doing stage $stage\n";
		if ($stage eq 'binutils') {
			install binutils => \@binutils, $bv
		} elsif ($stage eq 'c') {
			if ($do_headers) {
				install quick => \@kernel, $kv, qw/CC headers-only/;
				install headers => \@libs, $lv, qw/CC headers-only/;
				$preinstall = 1;
			}
			install stage1 => \@gcc, $gv, CC => bootstrap =>
				USE => [ @guse, @use, @guse_disable ]
		} elsif ($stage eq 'kernel') {
			install kernel => \@kernel, $kv, grep $preinstall, UPGRADE => 1
		} elsif ($stage eq 'libc') {
			install libc => \@libs, $lv, grep $preinstall, UPGRADE => 1
		} elsif ($stage eq 'cpp') {
			install stage2 => \@gcc, $gv,
				USE => [ @guse, @use, @guse_disable_stage2 ],
				IF_USE_CHANGED => 1;
			install extra_gcc => \@gcc, $gv, USE => [@guse, @use] if $extra_gcc;
			install extra_gdb => \@gdb, '', USE => [@guse, @use] if $extra_gdb;
		}
		last if $stage eq $max;
	}
}

setup_ctarget;
my $logfilename = "$cdev_logs/crossdev.$ctarget.".(time).".log";
open $logfile, '>', $logfilename or die ">$logfilename: $!";
select((select($logfile), $|=1)[0]);
do_stages;
close $logfile;
