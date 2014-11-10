package SequenceTools;
sub debug { }#print @_ }
use strict;
use warnings;
use base 'Exporter';
our (@EXPORT, @EXPORT_OK, %EXPORT_TAGS);
my %predef;
BEGIN {
	my @funcs = qw/&full_align_seq &align_seq &split_seq/;
	@EXPORT = @funcs;
	@EXPORT_OK = @EXPORT;
	my @predef = (
		[ qw/levenshtein leven lev/ ] => [
			score => \&lev_score,
			candidates => \&up_left_diag,
			init => \&init_lev,
			s_same => 1,
			s_diff => 0,
			s_insert => -1,
			s_delete => -1,
			s_gap => -1,
			ret => 'all',
		],
		[ qw/smithwaterman smith sw/ ] => [
			score => \&sw_score,
			candidates => \&sw_candi,
			init => \&init_z,
		],
		[ qw/needlemanwunsch needleman nw/ ] => [
			score => \&nw_score,
			candidates => \&up_left_diag,
			init => \&init_nw,
			s_gap => -1,
			s_same => 1,
			s_diff => 0,
		],
	);
	while (my ($labels, $val) = splice @predef, 0, 2) {
		for (@$labels) {
			$predef{$_} = $val;
			my $eval = "sub $_ { align_seq(\@_, type => q{$_}) }";
			eval $eval;
			$@ and die "eval(\"$eval\") failed: $@\n";
			push @EXPORT_OK, '&'.$_;
		}
	}
	%EXPORT_TAGS = ( ':all' => [ @EXPORT_OK ] );
}

use Data::Dumper;
use List::Util qw/min max/;
use Getopt::Long qw/:config pass_through/;

my $comparator;
BEGIN {
	# use a nice default comparator if it's available
	if (eval "use QGram") {
		QGram->import;
		$comparator = \&qgram;
	} else { # else just check for equality
		$comparator = sub { ($_[0] eq $_[1]) ? 1 : 0 };
	}
}

{
	my @patterns = (
		[ qw/letters l/ ] => [ qr//, { space => 1 } ],
		[ qw/spaces s/ ] => [ qr/\ / ],
		[ qw/punc p/ ] => [ qr/\b|([-;:\"\'\.+*&^%\$\#@!~+=\\\{\}\[\]\(\)])/ ],
		[ qw/words w/ ] => [ qr/\b/ ],
	);
	my (%patterns, %options);
	while (my ($keys, $info) = splice @patterns, 0, 2) {
		my ($pat, $opt) = @$info;
		$opt ||= {};
		$patterns{$_} = $pat for @$keys;
		$options{$_} = $opt for $pat, @$keys;
	}
	$options{$_}{defined} = 1 for keys %options;
	$options{$_}{trim} = 1 for keys %options;
	$options{$_}{length} = 1 for keys %options;
	sub split_seq {
		my ($pat, $string, %opt) = @_;
		$pat = $patterns{$pat} if not ref $pat;
		%opt = (%{$options{$pat||''}||{}}, %opt);
		my @ret = split /$pat/, $string;
		@ret = grep defined, @ret if $opt{defined};
		@ret = map { s/^\s+//, s/\s+$//; $_ } @ret if $opt{trim};
		@ret = grep /\S/, @ret unless $opt{space};
		@ret = grep length, @ret if $opt{length};
		@ret
	}
}

sub init_nw {
	my ($n, $m, $h, %var) = @_;
	$$h[$_][0] = ($var{s_gap}||0) * $_ for 0..$n;
	$$h[0][$_] = ($var{s_gap}||0) * $_ for 0..$m;
}

sub init_lev {
	my ($n, $m, $h, %var) = @_;
	$$h[$_][0] = 0 - $_ for 0..$n;
	$$h[0][$_] = 0 - $_ for 0..$m;
}

sub init_z {
	my ($n, $m, $h) = @_;
	for my $i (0..$n) {
		for my $j (0..$m) {
			$$h[$i][$j] = 0;
		}
	}
}

sub lev_score {
	my %var = @_;
	my ($h, $k, $ss, $tt, $comp) = @var{qw/h k ss tt comparator/};
	debug "comp($ss,$tt)", $comp->($ss,$tt),"\n";
	$k or return 0;
	return $h + ($comp->($ss, $tt) ? $var{s_same} : $var{s_diff}) if $k < 0;
	$h + $k * $var{s_gap};#$k * $var{s_gap}
}

sub sw_score {
	my %var = @_;
	my $r = 0;
	my ($h, $k, $ss, $tt, $comp) = @var{qw/h k ss tt comparator/};
	if ($k < 0) { # sim
		$r = $h + ($comp->($ss,$tt)||(-1/3));
	} elsif ($k) {
		$r = $h - (1 + $k/3);
	} else {
		$r = 0;
	}
	$var{debug} and warn "SCORE(@{[%var]})=$r\n";
	$var{debug} and warn Dumper [%var,$r];
	debug "score = $r\n";
	$r
}

sub nw_score {
	my %var = @_;
	my ($h, $k, $ss, $tt, $comp) = @var{qw/h k ss tt comparator/};
	die "NO H: @{[%var]}\n" unless defined $h;
	my $r = $h + $k * ($var{s_gap}||0);
	if ($k < 0) {
		debug "Not default score\n";
		my $sim = $comp->($ss,$tt);
		$r = $h + ($sim ? ($sim * $var{s_same}) : $var{s_diff});
	}
	$r;
}

sub generic_score {
	my %var = @_;
	my ($i, $j, $h, $k, $ss, $tt, $comp, $move) =
		@var{qw/i j h k ss tt comparator movetype/};
	my $factor = $var{mult} ? $k : 1;
	if ($k < 0) {
		$h += ($comp->($ss, $tt) ? $var{s_same} : $var{s_diff});
	} elsif ($k) {
		$h += $k * ($var{s_gap}||0);
		$h += ($move eq '-') ? $var{s_delete} : $var{s_insert};
	} else {
		$h = $var{s_init} || 0;
	}
}

sub up_left_diag {
	my %var = @_;
	my ($i, $j, $n, $m) = @var{qw/i j n m/};
	return
		grep { !grep $_ < 0, @$_[0,1] }
		[ $i-1, $j-1, -1, '\\' ],
		[ $i-1, $j,    1, '|'  ],
		[ $i,   $j-1,  1, '-'  ],
}

sub sw_candi {
	my %var = @_;
	my ($i, $j, $n, $m) = @var{qw/i j n m/};
	return
		grep { !grep $_ < 0, @$_[0,1] }
		[ $i-1, $j-1, -1, '\\' ],
		[ $i, $j, 0, '^' ],
		map([ $i-$_, $j, $_, '|' ], 1..$i-1),
		map([ $i, $j-$_, $_, '-' ], 1..$j-1),
}

my %defaults;
BEGIN {
	%defaults = (
		init => \&init_z,
		score => \&generic_score,
		s_insert => -1,
		s_delete => -1,
		s_same => 2,
		s_diff => -1,
		s_gap => -1,
		s_init => 0,
		candidates => \&up_left_diag,
		split => 'letters',
		comparator => $comparator,
		ret => 'sequence', # could also be 'indices'
		gap => '-',
	);
}

sub _do_split {
	my ($sin,$tin,%opt) = @_;
	my $split = $opt{split}||'l';
	my $splitter = ('CODE' eq ref $split)
		? $split
		: sub { split_seq($split,@_) };
	map 'ARRAY' eq ref($_) ? $_ : [ $splitter->($_) ], $sin, $tin;
}
sub _gap_type {
	my (%opt) = @_;
	my $ind = (($opt{ret}||'') =~ /^ind/i) ? 1 : 0;
	(exists $opt{gap}) ? $opt{gap} : $ind ? undef : '-';
}
sub _array_with_gaps {
	my ($arr, $ind, $gap) = @_;
	[map defined()?$$arr[$_]:$gap, @$ind]
}

sub _do_indices {
	my ($do, $s, $t, $sr, $tr) = @_;
	if ($do) {
		for (
			[$s,$sr],
			[$t,$tr],
			) {
			my ($orig, $ret) = @$_;
			my %map;
			$map{$$orig[$_]} = $_ for 0..$#$orig;
			@$ret = map $map{$_}, @$ret;
		}
	}
	die "REturning: [@$sr][@$tr]\n";
	return [@$sr], [@$tr]
}

sub full_align_seq {
	my ($sin,$tin,%opt) = @_;
	my $do_ind = ($opt{ret}||'') =~ /^ind/i ? 1 : 0;
	my ($s, $t) = _do_split(@_);
	my (@sr, @tr);
	while (@$s and @$t) {
		my ($salign, $talign) = align_seq($s,$t,%opt,ret=>indices=>);
#		use Data::Dumper; $Data::Dumper::Indent=1;die Dumper [map[map { (ref()||'')=~/XML/?$_->sprint:$_}@$_],$s,$t,\@sr,\@tr],\%opt,$salign,$talign if grep !defined, $salign, $talign;
		$_||=[]for$salign,$talign;
		last unless @$salign or @$talign;
		for (
			[$s,\@sr,$salign],
			[$t,\@tr,$talign],
			) {
			my ($orig, $ret, $align) = @$_;
			my @add = @$align;
			if (!$do_ind) {
				@add = map {
					(defined() and /^\d+$/)
					? $$orig[$_]
					: undef
				} @add;
			}
			push @$ret, @add;
			splice @$orig, $_, 1 for reverse sort { $a <=> $b } grep /^\d+$/, grep defined, @$align;
		}
	}
	[@sr],[@tr];
}

sub old_full_align_seq {
	my ($sin, $tin, %opt) = @_;
	my $gap = _gap_type(%opt);
	my $overall_indices = ($opt{ret}||'') =~ /^ind/i;
	delete $opt{gap} if exists $opt{gap};
	debug "GAP IS {",(defined($gap)?$gap:"(undef)"),"}\n";
	my ($s, $t) = _do_split($sin,$tin,%opt);
	if (!grep 0+@$_, $s, $t) {
		return _do_indices $overall_indices, $s, $t, [], []
	} elsif (!@$s) {
		return _do_indices $overall_indices, $s, $t, [map $gap, @$t], [@$t]
	} elsif (!@$t) {
		return _do_indices $overall_indices, $s, $t, [@$s], [map $gap, @$s]
	}

	$_||=[] for my ($salign, $talign) = align_seq($s,$t,%opt,gap=>undef,ret=>'indices');
	die "[@$s] [@$t] => {$salign} {$talign} death\n" if grep !ref, $salign, $talign;
	my (@spre, @smid, @spost, @sgood);
	my (@tpre, @tmid, @tpost, @tgood);
	for (
		[$s,$salign,\@spre,\@smid,\@spost,\@sgood],
		[$t,$talign,\@tpre,\@tmid,\@tpost,\@tgood],
		) {
		my ($in, $al, $pre, $mid, $post, $good) = @$_;
		@$pre = 0..$#$in and next if !@$al;
		my %good = map { ($_ => 1) } grep defined, @$al;
		my $min = min keys %good;
		my $max = max keys %good;
		@$pre = 0..$min-1;
		@$post = $max+1..$#$in;
		@$good = @$al;
		@$mid = grep !$good{$_}, $min..$max;
	}
	my (@sr, @tr);
	if (!@sgood and !@tgood) {
		debug "No good [@$s] [@$t]\n";
		@sr = (@spre,@smid,@spost);
		@tr = map undef, @sr;
		push @tr, @tpre,@tmid,@tpost;
		push @sr, undef while @sr < @tr;
		return _do_indices $overall_indices, $s, $t, map _array_with_gaps(@$_), [$s,\@sr,$gap], [$t,\@tr,$gap]
	}
	my (%sdone, %tdone);
	for (
		[pre=>\@spre,\@tpre,1],
		[good=>\@sgood,\@tgood],
		[mid=>\@smid,\@tmid,1],
		[post=>\@spost,\@tpost,1],
		) {
		my ($debug, $sar, $tar, $align) = @$_;
#		for (
#			[$sar,\%sdone],
#			[$tar,\%tdone],
#			) {
#			my ($arr,$done) = @$_;
#			next unless @$arr;
#			my $min = min grep defined, @$arr;
#			my $max = max grep defined, @$arr;
#			@$arr = grep $$done{$_}?undef:$_, $min..$max;
#			$$done{$_}++ for @$arr;
#		}
		my ($sadd, $tadd) = map _array_with_gaps(@$_), [$s,$sar,$gap], [$t,$tar,$gap];
		($sadd, $tadd) = full_align_seq($sadd, $tadd, %opt) if $align;
		push @sr, @$sadd;
		push @tr, @$tadd;
		debug "$debug [@$sadd] [@$tadd]\n";
	}
	_do_indices $overall_indices, $s, $t, [@sr],[@tr]
}

sub align_seq {
	my ($sin, $tin, %opt) = @_;
	%opt = (%defaults, %opt);
	%opt = (%opt, @{$predef{$opt{type}}||[]}) if $opt{type};
	my ($init_h, $score, $candi) = (\&init_z, \&generic_score, \&up_left_diag);
	for (
		[ init => \$init_h ],
		[ score => \$score ],
		[ candidates => \$candi ],
		) {
		my ($key, $ref) = @$_;
		next unless $opt{$key};
		$$ref = $opt{$key};
	}
	my ($s, $t) = _do_split($sin,$tin,%opt);
	my ($n, $m) = map 0+@$_, $s, $t;
	my $ret_type = $opt{ret} || '';
	my $force_all = ($ret_type =~ /^all/i or $opt{full}) ? 1 : 0;
	#debug "PARAMS: @{[%opt]}\nS=@$s\nT=@$t\n";
	my @h;
	if ($force_all) {
		$h[0][0] = 0;
	} else {	
		$init_h->($n, $m, \@h, %opt);
	}
	my @t = map ref($_) ? [map '^', @$_] : '^', @h;
	my @i;
	my $order = '\\^|-';
	for my $i (($force_all?0:1)..$n) {
		for my $j (($force_all?0:1)..$m) {
			next unless $i or $j;
			$i[$i][$j] = [ $i, $j ];
			my @cand = map {
				my ($I, $J, $K, $T) = @$_;
				[
					$I,
					$J,
					$score->(
						%opt,
						i => $I, j => $J, k => $K, movetype => $T,
						h => $h[$I][$J],
						ss => $i?$$s[$i-1]:'', tt => $j?$$t[$j-1]:'',
					),
					$T
				];
			} $candi->(i => $i, j => $j, n => $n, m => $m, %opt);
			debug map "$_\n", "comparing($i,$j,$$s[$i-1],$$t[$j-1]) N=".(0+@cand), map join(',',@$_), @cand;
			@cand = sort {
				$$b[2] <=> $$a[2] # score descending
				or index($order,$$a[3]) <=> index($order,$$b[3]) # type
				or abs($$a[0]-$$a[1]) <=> abs($$b[0]-$$b[1]) # close to diag
				or grep(!$_, @$a[0,1]) <=> grep(!$_, @$b[0,1]) # non-zeros
				or ($$b[0]+$$b[1]) <=> ($$a[0]+$$a[1]) # sum of indices
				or ((($$a[0] > $$b[0]) and ($$a[1] > $$b[1])) ? -1 : 0)
				or ((($$b[0] > $$a[0]) and ($$b[1] > $$a[1])) ? 1 : 0)
			} @cand;
			debug map "$_\n", "results", map join(',',@$_), @cand;
			my $best = shift @cand;
			$i[$i][$j] = [ @$best[0,1] ]; # backtrace indices
			$h[$i][$j] = $$best[2]; # score
			$t[$i][$j] = $$best[3]; # type
		}
	}
	return $h[$n][$m] if $ret_type =~ /^(?:full|final)val(?:ue)?$/i;
	my ($besti, $bestj, $besth) = (0,0,0);
	for my $i (0..$n) {
		for my $j (0..$m) {
			my $h = $h[$i][$j];
			if ($h > $besth # if it's the best outright
				or ($h == $besth # tie goes to the one closer to the diagonal
					and (abs($i-$j) < abs($besti-$bestj)
						or $i+$j > $besti+$bestj)
					)
				) {
				($besti, $bestj, $besth) = ($i, $j, $h);
			}
		}
	}
	return $besth if $ret_type =~ /^bestval(?:ue)?$/i;
	# form the sequence through backtraces
	my @ind = $force_all ? ([$n,$m]) : ([$besti, $bestj]);
	my @typ;
	while (1) {
		my ($li, $lj) = @{$ind[0]};
		my $back = $t[$li][$lj];
		unshift @typ, $back;
		last if $back eq '^';
		my ($bi, $bj) = @{$i[$li][$lj]};
		unshift @ind, [$bi, $bj];
	}
	# weed out odd patterns (spurious insertions/deletions)
	for my $i (reverse 1..$#ind-1) {
		next if $ind[$i+1][0]-$ind[$i-1][0] != 1;
		next if $ind[$i+1][1]-$ind[$i-1][1] != 1;
		splice @ind, $i, 1;
		splice @typ, $i-1, 1;
	}

	if ($opt{dump}) {
		print Dumper $s, $t, $n, $m, \@h;
		my @H = map [map sprintf("%4.1f",$_), @$_], @h;
		my @T = map [@$_], @t;
		$H[$$_[0]][$$_[1]] = "\e[31m$H[$$_[0]][$$_[1]]\e[0m" for @ind;
		$T[$$_[0]][$$_[1]] = "\e[31m$T[$$_[0]][$$_[1]]\e[0m" for @ind;
		print "         @{[map qq/ {$_}/, @$t]}\n";
		my @sd = map "{$_}", @$s;
		unshift @sd, '   ';
		print "$sd[$_] @{$H[$_]}\n" for 0..$#H;
		print @{$T[$_]},"\n" for 0..$#T;
	}

	# sequence indices are off by one
	@$_ = map $_-1, @$_ for @ind;
	# print map "[@$_]", @ind;
	# print "\n@typ\n";
	my $ind = ($ret_type =~ /^ind/i) ? 1 : 0;
	my $gap = _gap_type(%opt);
	my @seq;
	for my $off (1..$#ind) {
		my $typ = $typ[$off];
		my ($si, $ti) = @{$ind[$off]};
		push @{$seq[0]}, ($typ eq '-') ? $gap : $ind ? $si : $$s[$si];
		push @{$seq[1]}, ($typ eq '|') ? $gap : $ind ? $ti : $$t[$ti];
	}
	@seq;
}
1;
