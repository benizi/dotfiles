#!/usr/bin/perl
use strict;
use warnings;
use File::Copy;
use File::Find;
use File::Spec;
use Getopt::Long;
my $perl_dir = "/root/FAKEROOT-FOR-PERL";
GetOptions(
	'directory=s' => \(my $dir = ''),
	'perl' => \(my $is_perl = '.'),
	'groupy|groupw|gw!' => \(my $match_group = 0),
	'uid|user|owner=s' => \(my $owner = ''),
	'gid|group=s' => \(my $group = ''),
) or die;
$dir ||= $is_perl ? $perl_dir : '.';
$_ = File::Spec->rel2abs($_) for $dir;
$_ = /\D/ ? (getpwnam $_)[2] : (getpwuid $_)[2] for grep length, $owner;
die "Unknown username/userid\n" unless defined $owner;
$_ = /\D/ ? (getgrnam $_)[2] : (getgrgid $_)[2] for grep length, $group;
die "Unknown groupname/groupid" unless defined $group;
s/^$/-1/ for $owner, $group;
my $do_chown = grep $_ >= 0, $owner, $group;

sub match_perms {
	my $this = shift;
	my $u = ($this & 0700) >> 6;
	my $g = ($this & 0070) >> 3;
	my $o = ($this & 0007);
	$g |= ($u & ($match_group ? 07 : 05));
	$o |= ($u & 05);
	my $new = ($this & 07000) | ($u << 6) | ($g << 3) | $o;
}

find sub {
	my $fn = $File::Find::name;
	(my $rel = $fn) =~ s{^\Q$dir\E/?}{};
	my $abs = "/$rel";
	chown $owner, $group, $_ if $do_chown;
	if (-d and -e $abs) {
		my $abs_mode = (stat $abs)[2] & 07777;
		chmod $abs_mode, $_;
	} elsif (-d or -f) {
		my $this_mode = (stat)[2] & 07777;
		my $new_mode = match_perms $this_mode;
		return if $this_mode == $new_mode;
#		printf "%04o -> %04o %s\n", $this_mode, $new_mode, $abs;
		chmod $new_mode, $_;
	} elsif (-l) {
		warn "No action for link: $abs (".(readlink).")\n";
	} else {
		warn "Unknown action for $abs\n";
	}
	if ($is_perl and /^\.packlist$/) {
		my $new = '.packlist.new';
		{
			open my $f, '>', $new or die ">$new: $!";
			local @ARGV = ($_);
			local $_;
			while (<>) {
				s{\Q$perl_dir\E}{}g;
				print $f $_;
			}
			close $f;
		}
		chown((stat)[4,5], $new);
		move($new, $_);
	}
	#exit if -f;
}, $dir;
