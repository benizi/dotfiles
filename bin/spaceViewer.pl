#!/usr/bin/env perl
use strict;
use warnings;

use Getopt::Long;
Getopt::Long::Configure(qw/bundling/);
my (@dirs, @exclude);
my ($width, $height) = (900, 600);
my $rc = "$ENV{HOME}/.spaceViewerrc";
Getopt::Long::Configure(qw/pass_through/);
GetOptions( 'rc=s' => \$rc ) or die "Error parsing rc file\n";
if (-f $rc) {
	my $txt = do {
		open my $f, '<', $rc or die "<$rc: $!";
		undef local $/;
		<$f>
	};
	eval $txt;
	warn "$@\n" if $@;
}
Getopt::Long::Configure(qw/no_pass_through/);
GetOptions(
	'<>' => sub { my $s if 0; @dirs = () unless $s; $s ||= 1; push @dirs, @_; },
	'otherdev' => \ (my $otherdev = 0),
	'file=s' => \ (my $file = "$ENV{HOME}/spaceViewer-saved"),
	'gen|genfile' => \ (my $genfile = 0),
	'F' => \ (my $files = 0),
	'w|width=i' => \$width,
	'h|height=i' => \$height,
	'mf|minfile=i' => \ (my $mf = 10), # ignore files less than 10 bytes
	'md|mindir=i' => \ (my $md = 100), # ignore dirs  less than 100 bytes total
	's|scale=i' => \ (my $scale = 10000),
	'gray|grey' => \ (my $gray = 0),
	'exclude=s' => \@exclude,
	'graphic|tk|Tk!' => \ (my $Tk = 1),
	'sort=s' => \ (my $sort = 'file'), # could be 'size'
	'debug+' => \(my $debug = 0),
) or die 'options';
my %sort = (
	'file' => sub { sort { $$a[0] cmp $$b[0] } @_ },
	'size' => sub { sort { $$b[1] <=> $$a[1] or $$a[0] cmp $$b[0] } @_ },
);
$sort{name} = $sort{file};
die "--sort must be one of: ", join(', ', keys %sort), "\n" unless $sort{$sort};

use Storable qw/nstore retrieve/;
use File::Find;
for (
	[$Tk, 'Tk'],
	[1, qw/Time::HiRes time/],
	) {
	my ($use, $m, @args) = @$_;
	next if !$use;
	eval "use $m; 1" or (warn "While including $m:\n$@\n" and next);
	$m->import(@args);
}

my %exclude = map { $_ => 1 } @exclude;
$_ /= $scale for $mf, $md;
my %files;
my %seen;
sub radd { # recursive add - generate tree with 
	my $s = shift;
	return $s->{total} if exists $s->{total};
	my $t = 0;
	$t += radd($s->{dirs}{$_}) for keys %{$s->{dirs}};
	my $f = 0;
	$f += $s->{files}{$_} for keys %{$s->{files}};
	$s->{justfiles} = $f;
	$t += $f;
	$s->{total} = $t;
	$t;
}

if (($genfile and not @dirs) and !-f $file) {
	print "Nothing to do. (Specify directories and --gen or stored file)\n";
	exit;
}

if ($genfile or not -f $file) {
	# on interrupt, print status. If interrupted again w/in 1 second, quit.
	my $sigint = $SIG{INT} || sub { CORE::exit; };
	my $t = time-10;
	my $status = '';
	local $SIG{INT} = sub {
		&$sigint if time - $t < 1;
		$t=time;
		print $status, $/;
	};

	for my $start (@dirs) {
		print "Searching <$start>\n";
		my $dev = (stat $start)[0];
		print STDERR "Finding files";
		my $c = 0;
		find {
			wanted => sub {
				print STDERR "." unless $c % 1000;
				$c++;
				return unless -f;
				return if -l;
				$status = $File::Find::dir;
				my $d = \%files;
				$d = ($d->{dirs}{$_} || ($d->{dirs}{$_} = {}))
				for grep { $_ } split m|/|, $File::Find::dir;
				($files ? $d->{dirs}{$_}{total} : $d->{files}{$_}) = -s;
			},
			preprocess => sub {
#				use Data::Dumper; die Dumper \%exclude, $File::Find::dir, \@_;
#				grep print("$_$/"),
				grep {
					not $exclude{"$File::Find::dir/$_"} and
					not $exclude{"$File::Find::dir$_"} }
				grep { not $seen{"$File::Find::dir/$_"}++; }
				grep { $otherdev or ($dev == ((stat $_)[0] || 0)); }
				@_;
			},
		}, $start;
		warn "\n";
	}
	print "Total: ".radd(\%files)."\n";
} else {
	%files = %{retrieve $file};
}
nstore \%files, $file if $file and $genfile;
if (!$Tk) {
	print "Done\n";
	exit;
}
my $mw = MainWindow->new;
my $C = $mw->Canvas(width => $width, height => $height)->pack;
my $T = '';
$mw->Entry(-textvariable => \$T, -background => 'white')->pack(qw/-side bottom -fill x/);
my @colors = $gray
	? (map { '#'.(sprintf "%02x%02x%02x", map { $_ % 256 }
				  ($_ * 0xff/20, $_ * 0xff/20, $_ * 0xff/20)) } 0..20)
	: (qw/red green blue yellow/);
my $TAG = 0;
rc();
sub rc { # recursive canvas
	my @d = (k => '/', d => \%files, xm => 0, xM => $width,
			 ym => 0, yM => $height, v => 1, c => 0);
	my %opts = (@d, @_);
	my ($k, $d, $xm, $xM, $ym, $yM, $v, $c) = @opts{qw/k d xm xM ym yM v c/};
	my $tot = ref($d) ? $$d{total} : $d;
	$d = 0 unless ref $d;
	$debug and print '{ ', join(', ', map { "$_ => $opts{$_}" } sort keys %opts), " }\n";
	my ($xd, $yd) = map $opts{$_.'M'} - $opts{$_.'m'}, qw/x y/;
	$TAG++;
	my $tag = 't'.$TAG;
	my $color = $colors[$c % @colors];
	my @subthings = !$d ? () : map [ $_, $$d{dirs}{$_}{total} ], keys %{$$d{dirs}};
	if ($d and $files) {
		my @files = map [ $_, $$d{files}{$_} ], keys %{$$d{files}};
		@files = grep $$_[1] > 100_000, @files;
		@files = @files[0..19] if @files > 20;
		push @subthings, @files;
	}
	@subthings = $sort{$sort}->(@subthings);
	my $t = $v ? $xm : $ym;
	if ($tot) {
		for (@subthings) {
			my ($name, $size) = @$_;
			next unless $size;
			my $delt = ($v ? $xd : $yd) * ($size/$tot);
			my @o = (
				k => ($k eq '/') ? $k.$name : "$k/$name",
				d => ($$d{dirs}{$name}||$size), c => ($gray ? ($c + 1) : ++$c),
				($v
				 ? (xm => $t, xM => $t + $delt, ym => $ym, yM => $yM)
				 : (ym => $t, yM => $t + $delt, xm => $xm, xM => $xM)),
				v => ($v ? 0 : 1)
			);
			rc(@o);
			$t += $delt;
		}
	}
	$v ? ($xm = $t) : ($ym = $t);
	return if $xm >= $xM or $ym >= $yM;
	my $r = $C->createRectangle(
		$xm, $ym,
		$xM, $yM,
		-fill => $color,
		-tag => $tag
	);
	$C->bind($r, '<Enter>', sub { $T = "$k       (".($tot/($xd*$yd))." bpp) ($tot) = ".human($tot) });
	$C->bind($r, '<Button-1>', sub { $C->delete; rc(d => $d, v => $v, k => $k, c => $c); });
	$C->raise($tag);
}
$mw->bind('<Button-3>', sub { $C->delete; rc(); });
$mw->bind('<Key-q>', sub { Tk::exit(); });
my %keys = (
	G => 1024**3,
	M => 1024**2,
	K => 1024,
);
sub human {
	my $v = shift;
	$v > $keys{$_} and return sprintf "%.2f%s", $v / $keys{$_}, $_ for sort { $keys{$b} <=> $keys{$a} } keys %keys;
	$v;
}
MainLoop();
