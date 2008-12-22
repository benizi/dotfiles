#!/usr/bin/perl
# original: http://www.angelfire.com/darkside/wbz2jpg/wbz2jpg.c
use strict;
use warnings;
use bytes;
use Getopt::Long qw/:config pass_through/;
GetOptions(
	'input|infile=s' => \(my $infile = ''),
	'output|outfile=s' => \(my $outfile = ''),
	'find=s' => \(my $find_dir = ''),
) or die 'options';
use File::Find;
$infile ||= shift;
die 'options' if @ARGV;
if ($find_dir) {
	find {
		wanted => sub {
			return unless /\.wb[01c]$/;
			my $in = $File::Find::name;
			return if grep -f,
				"$in.jpg",
				map sprintf("%s.%0*d.jpg", $in, $_, 0),
				1..4;
			print "$in\n";
			my $ret = system { $^X } $^X, $0,
				'--infile', $in,
				'--outfile', "$in.jpg";
			$ret and die "$in\n";
		},
		preprocess => sub { sort @_ },
	}, $find_dir;
	exit;
}

my $COLLECTION = "\xab\x16\xfa\x95";
my $NORMAL_IMG = "\xe2\xcd\x71\xf0";
my $JFIF_MARKER= "\xff\xd8\xff\xe0";
{
	undef local $/;
	local @ARGV = grep $_, $infile;
	$_ = <>;
}
my @collection;
sub myodf {
	my $f = shift;
	return;
	open my $od, "| myodc -I -f $f" or die "|myodc: $!";
	print $od @_;
	close $od;
}
sub myod { myodf 0, @_; }
{
	if (/^WWBB(\d)\1\1\1/) {
		@collection = ($_);
	} elsif (/^\Q$COLLECTION\E/) {
		open my $in, '<', \$_ or die "<ref: $!";
		myod(substr $_, 0, 16);
		my $off = unpack 'I', substr $_, 4, 4;
		my $total = length;

		while ($off < $total) {
			my $hdr_info = substr $_, $off, 12;
			my $type = substr $hdr_info, 0, 4;
			my ($hdr_len, $len) = unpack 'II', substr $hdr_info, 4;
			myod($hdr_info);
			myodf($off, substr $_, $off, 64);
			my @errors;
			push @errors, ($type ne $NORMAL_IMG) ? 1 : 0;
			push @errors, ($off+$hdr_len > $total) ? 1 : 0;
			if (grep $_, @errors) {
				my $errfile = $infile . '.err';
				open my $err, '>', $errfile or die ">$errfile: $!";
				print $err "@errors\n";
				close $err;
				last;
			}
			my $img_data = substr $_, $off+$hdr_len, $len;
			myodf($off+$hdr_len, substr $img_data, 0, 16);
			push @collection, $img_data;
			$off += $len;
		}
		warn "FOUND ", 0+@collection, " imgs in $infile\n";
	}
}
die "Couldn't find photos\n" unless @collection;

if (@collection > 1) {
	$outfile ||= $infile . '.jpg' if $infile;
	die "Must specify --outfile=BASE for collections\n" unless $outfile;
	$outfile =~ s/$/\.jpg/ unless $outfile =~ /.\.\w\w\w\w?$/;
}

my %decode_const = (
	'1111' => 0xf2,
	'0000' => 0xa4,
);

sub wb2jpeg {
	local $_ = shift;
	return $_ if /^\Q$JFIF_MARKER\E/;
	die "Bad WWBB version?\n" unless s/^WWBB(0000|1111)//;
	my $decode_xor = $decode_const{$1};
	my $rest=substr$_,200;
	my @a=split//,substr $_, 0, 100;
	my @c=(my @b=split//,substr $_,100,100);
	my @d;
	while (@a) {
		my ($A, $B) = map ord shift @$_, \@a, \@b;
		push @d, map chr, ($B ^ (255-$A)) ^ $decode_xor;
	}
	join '', @d, @c, $rest;
}

my $N = @collection;
my $l = length $N;
for my $i (0..$#collection) {
	my $out = *STDOUT;
	my $outfn = $outfile ? $outfile : "";
	$outfn =~ s/(\.\w\w\w\w?)$/sprintf ".%0*d%s", $l, $i, $1/e if $N > 1;
	open $out, '>', $outfn if $outfn;
	print $out wb2jpeg $collection[$i];
}
__END__
perl -nwe 'BEGIN{undef$/} die "WWBB" unless s/^WWBB(0000|1111)//; $magic = ($1 eq "1111") ? 0xf2 : 0xa4; $jpg=substr$_,200; @a=split//,substr $_, 0, 100; @c=(@b=split//,substr $_,100,100); while (@a) { $a=ord shift@a; $b=ord shift@b; push @d, map chr, ($b ^ (255-$a)) ^ $magic; } print @d, @c, $jpg' $l | display jpg:-
