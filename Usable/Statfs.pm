package Statfs;
use strict;
use warnings;
use Exporter;
use Config;
our @ISA = 'Exporter';
our @EXPORT;
our $INT = 1;
BEGIN { @EXPORT = map '&statfs'.$_, '', qw/K M G/; }
our @EXPORT_OK = @EXPORT;
our %alias;
my @order;
my %order;
BEGIN {
	for (
		[qw/type fstype magic/],
		[qw/bsize optimal transfer transfersize/],
		[qw/blocks totalblocks totblocks total tot/],
		[qw/bfree freeblocks free/],
		[qw/bavail available avail availblocks availableblocks/],
		[qw/files totfiles totalfiles filenodes/],
		[qw/ffree freefiles freefilenodes/],
		[qw/fsid/],
		[qw/fsid2/],
		[qw/namelen namelength nlength/],
		[qw/extra/],
		) {
		my ($standard, @rest) = @$_;
		$alias{$_} = $standard for @$_;
		push @order, $standard;
		$order{$standard} = $#order;
	}
}
sub statfsG { statfs(@_?(@_,2**20):($_,2**20)) }
sub statfsM { statfs(@_?(@_,2**20):($_,2**20)) }
sub statfsK { statfs(@_?(@_,2**10):($_,2**10)) }
sub statfs {
	require 'syscall.ph';
	my $use64 = $Config{use64bitall} ? 0 : 1;
	my $long = $Config{ivsize} != 4 ? 1 : 0;
	my ($path, $block) = @_ ? @_ : ($_);
	return unless grep length, grep defined, $path;
	$path = sprintf "%s", $path; # can't pass a read-only value to syscall
	$block ||= 1024;
	my $stat = '0' x 128; # pre-extend the buffer
	my @vals;
	if ($long and $use64) {
		warn "statfs failed: $!\n" and return if syscall &SYS_statfs64, $path, $stat;
#	} elsif (FAKELONG) {
#		for (reverse map $_*2, 1..5) {
#			my ($l, $h) = map abs, splice @vals, $_, 2;
#			$l += $h << 32 if $h;
#			splice @vals, $_, 0, $l;
#		}
	} else {
		warn "statfs failed: $!\n" and return if syscall &SYS_statfs, $path, $stat;
		my $unpack = $long ? 'Q[16]' : 'l[16]';
		@vals = unpack $unpack, $stat;
		splice @vals, -5;
	}
	my $fs_block = $vals[1];
	$_ *= $fs_block / $block for @vals[2..4];
	@vals = map int, @vals if $INT;
	return @vals if wantarray;
	my $h = {};
	$$h{$order[$_]} = $vals[$_] for 0..$#vals;
	$$h{used} = $$h{blocks} - $$h{bfree};
	bless $h, __PACKAGE__;
}
sub fields { @order }
our $AUTOLOAD;
sub AUTOLOAD {
	(my $func = $AUTOLOAD) =~ s/^.*://;
	(my $field = $func) =~ tr/A-Z_/a-z/d;
	$field = $alias{$field} // $field;
	if (!exists ${$_[0]}{$field}) {
		my $extra = ($func eq $field) ? "" : " (-> $field)";
		die "No such field: $func$extra\n";
	}
	no strict 'refs';
	*$func = sub { shift->{$field} };
	goto &$func;
}
sub DESTROY {}
1;
