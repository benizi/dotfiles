package MyMatrices;
use strict;
use warnings;
use List::Util qw/min max/;
our @ISA = qw/Exporter/;
our (@EXPORT, @EXPORT_OK, %EXPORT_TAGS);
BEGIN {
	my @funcs =
		map { ("&load_$_","&save_$_") }
		qw/matlabs matlabd/,
		map { ($_.'_tr','diag_'.$_,'vec_'.$_,$_) }
		map { ($_.'D', $_) }
		qw/sb st db dt/;
	push @funcs,
		map { ("&read_$_", "&write_$_", "&net$_", "&unnet$_") }
		qw/N S f D/;
	push @funcs, '&open_or_die', '&open_or_die_out';
	push @funcs, '&prepend', '&curry', '&append';
	push @funcs, '&get_funcs_r', '&get_funcs_w';
	@EXPORT_OK = @funcs;
	$EXPORT_TAGS{all} = [ @funcs ];
	@EXPORT = @funcs;
}

sub prepend { my ($func, @args) = @_; return sub { &$_ for @args; &$func } }
sub curry { my ($func, @args) = @_; return sub { $func->(@args,@_) }; }
sub append { my ($func, @args) = @_; return sub { &$func; &$_ for @args } }

sub open_or_die     { _open_or_die(@_,0) }
sub open_or_die_out { _open_or_die(@_,1) }
sub _open_or_die {
#use Data::Dumper;print Dumper _ood => \@_;
	my $fn = shift;
	return $fn if ref $fn;
	my $write = shift;
	my $mode = $write ? '>' : '<';
	open my $fh, $mode, $fn or die "$mode$fn: $!";
	$fh;
}

sub netN ($) { pack 'N', $_[0] }
sub netI ($) { pack 'I', $_[0] }
sub netS ($) { pack 'S', $_[0] }
sub netf ($) { scalar reverse pack 'f', $_[0] }
sub netD ($) { pack 'd', $_[0] }

sub unnetN ($) { unpack 'N', $_[0] }
sub unnetI ($) { unpack 'I', $_[0] }
sub unnetS ($) { unpack 'S', $_[0] }
sub unnetf ($) { unpack 'f', scalar reverse $_[0] }
sub unnetD ($) { unpack 'd', $_[0] }

sub readV { my ($fh) = @_; my $n = (@_>1)?$_[1]:1;local $/ = \$n; scalar <$fh> }
sub read1 { my ($fh) = @_; local $/ = \1; scalar <$fh> }
sub read2 { my ($fh) = @_; local $/ = \2; scalar <$fh> }
sub read4 { my ($fh) = @_; local $/ = \4; scalar <$fh> }
sub read8 { my ($fh) = @_; local $/ = \8; scalar <$fh> }
my %buf;
sub read_space_separated {
	my $fh = shift;
	my $buf = ($buf{$fh}||=[]);
	if (!@$buf and defined(my $in = <$fh>)) {
		push @$buf, split ' ', $in;
	}
	shift @$buf;
}

sub read_N { unnetN &read4 }
sub read_I { unnetI &read4 }
sub read_S { unnetS &read2 }
sub read_f { unnetf &read4 }
sub read_D { unnetD &read8 }

sub write_N     { my ($fh, $v) = @_; print $fh netN $v }
sub write_I     { my ($fh, $v) = @_; print $fh netI $v }
sub write_S     { my ($fh, $v) = @_; print $fh netS $v }
sub write_f     { my ($fh, $v) = @_; print $fh netf $v }
sub write_D     { my ($fh, $v) = @_; print $fh netD $v }

sub write_Nt    { my ($fh, $v) = @_; print $fh $v }
sub write_It    { my ($fh, $v) = @_; print $fh $v }
sub write_St    { my ($fh, $v) = @_; print $fh $v }
sub write_ft    { my ($fh, $v) = @_; printf $fh "%.6g", $v }
sub write_Dt    { my ($fh, $v) = @_; printf $fh "%g", $v }

sub write_space { my ($fh, $v) = @_; print $fh " " }
sub write_CR    { my ($fh, $v) = @_; print $fh "\n" }
sub write_bytes { my ($fh, $v) = @_; print $fh $v }

# my %fillfunc = (
# 	N => \&write_N,
# 	S => \&write_S,
# 	f => \&write_f,
# 	D => \&write_D,
# );
# {
# 	my %locs
# 	sub tofill {
# 		my ($fh, $name, $type, $val) = @_;
# 		my $loc = tell $fh;	
# 
# 	}
# }

sub getOptions {
	my @params = (
		[ [qw/maxr mr rmax rowmax rm/]=>0,qr/\d+/ ],
		[ [qw/maxc mc cmax colmax cm/]=>0,qr/\d+/ ],
		[ [qw/name nm entry/]=>'',qr/^[a-z][a-z_0-9]*$/ ],
		[ [qw/date time/]=>'',qr/.*/ ],
	);
	my %parm;
	$parm{$$_[0][0]} = $$_[1] for @params;
	while (@_ > 1 and !grep ref, @_[0,1]) {
		my $found = 0;
		for (@params) {
			my ($flags, $default, $pattern) = @$_;
			if (grep $_ eq $_[0], @$flags) {
				$found++;
				shift;
				$parm{$$flags[0]} = shift;
				last;
			}
		}
		last unless $found;
	}
	%parm;
}

sub get_funcs_r { unshift @_, 'read';  &_get_funcs }
sub get_funcs_w { unshift @_, 'write'; &_get_funcs }
sub _get_funcs {
#use Data::Dumper;print Dumper _g_f => \@_;
#	warn "_GET_FUNCS(@_)\n";
	my ($read_write, $fh, $binary, $double) = splice @_, 0, 4;
#	warn "\@_ -> {@_}\n";
	my $write  = $read_write eq 'write' ? 1 : 0;
	my $read = $read_write eq 'read' ? 1 : 0;
	die "_get_funcs($read_write)??\n" if 1 != grep $_, $read, $write;
	$fh = &open_or_die($fh) if $read;
	$fh = &open_or_die_out($fh) if $write;
	my %funcs;
	if ($read_write eq 'read') {
		$funcs{N} = $binary ? \&read_N : \&read_space_separated;
		$funcs{I} = $binary ? \&read_I : \&read_space_separated;
		$funcs{f} = $binary ? \&read_f : \&read_space_separated;
		$funcs{D} = $binary ? \&read_D : \&read_space_separated;
		$funcs{S} = $binary ? \&read_S : \&read_space_separated;
		$funcs{1} = $binary ? \&readV  : \&readV;
	} elsif ($read_write eq 'write') {
		$funcs{N}     = $binary ? \&write_N     : \&write_Nt;
		$funcs{f}     = $binary ? \&write_f     : \&write_ft;
		$funcs{D}     = $binary ? \&write_D     : \&write_Dt;
		$funcs{S}     = $binary ? \&write_S     : \&write_St;
		$funcs{I}     = $binary ? \&write_I     : \&write_It;
		$funcs{bytes} = $binary ? \&write_bytes : \&write_bytes;
		$funcs{SPC}   = $binary ? sub{}         : \&write_space;
		$funcs{CR}    = $binary ? sub{}         : \&write_CR;
	} else {
		die "Bad get_funcs??";
	}
	$funcs{f} = $funcs{D} if $binary and $double;
	$funcs{seek} = sub { my $fh = shift; seek $fh, $_[0], 0 };
	$funcs{tell} = sub { my $fh = shift; tell $fh };
	$funcs{eof} = sub { my $fh = shift; eof $fh };
	map { $_, curry($funcs{$_},$fh) } keys %funcs;
}

my $miMATRIX = 14;
my $miINT8 = 1;
my $miINT32 = 5;
my $miUINT32 = 6;
my $miSINGLE = 7;
my $miDOUBLE = 9;
my $mxSPARSE_CLASS = 5;
my $mxDOUBLE_CLASS = 6;

# load_matrix(fn, bin, double, *, dense, transpose, matlab)
sub load_sb		    { my $fn = shift; load_matrix($fn,1,0,@_,0,0,0) }
sub load_st         { my $fn = shift; load_matrix($fn,0,0,@_,0,0,0) }
sub load_db         { my $fn = shift; load_matrix($fn,1,0,@_,1,0,0) }
sub load_dt         { my $fn = shift; load_matrix($fn,0,0,@_,1,0,0) }
sub load_sb_tr      { my $fn = shift; load_matrix($fn,1,0,@_,0,1,0) }
sub load_st_tr      { my $fn = shift; load_matrix($fn,0,0,@_,0,1,0) }
sub load_db_tr      { my $fn = shift; load_matrix($fn,1,0,@_,1,1,0) }
sub load_dt_tr      { my $fn = shift; load_matrix($fn,0,0,@_,1,1,0) }
sub load_sbD	    { my $fn = shift; load_matrix($fn,1,1,@_,0,0,0) }
sub load_stD        { my $fn = shift; load_matrix($fn,0,1,@_,0,0,0) }
sub load_dbD        { my $fn = shift; load_matrix($fn,1,1,@_,1,0,0) }
sub load_dtD        { my $fn = shift; load_matrix($fn,0,1,@_,1,0,0) }
sub load_sbD_tr     { my $fn = shift; load_matrix($fn,1,1,@_,0,1,0) }
sub load_stD_tr     { my $fn = shift; load_matrix($fn,0,1,@_,0,1,0) }
sub load_dbD_tr     { my $fn = shift; load_matrix($fn,1,1,@_,1,1,0) }
sub load_dtD_tr     { my $fn = shift; load_matrix($fn,0,1,@_,1,1,0) }
sub load_matlabs    { my $fn = shift; load_matrix($fn,1,1,@_,0,0,1) }
sub load_matlabd    { my $fn = shift; load_matrix($fn,1,1,@_,1,0,1) }
sub load_matlabs_tr { my $fn = shift; load_matrix($fn,1,1,@_,0,1,1) }
sub load_matlabd_tr { my $fn = shift; load_matrix($fn,1,1,@_,1,1,1) }

# load_vector(fn, bin, double, *, dense, diagonal, transpose)
sub load_diag_sb    { my $fn = shift; load_vector($fn,1,0,@_,0,1,0) }
sub load_diag_st    { my $fn = shift; load_vector($fn,0,0,@_,0,1,0) }
sub load_diag_db    { my $fn = shift; load_vector($fn,1,0,@_,1,1,0) }
sub load_diag_dt    { my $fn = shift; load_vector($fn,0,0,@_,1,1,0) }
sub load_diag_sbD   { my $fn = shift; load_vector($fn,1,1,@_,0,1,0) }
sub load_diag_stD   { my $fn = shift; load_vector($fn,0,1,@_,0,1,0) }
sub load_diag_dbD   { my $fn = shift; load_vector($fn,1,1,@_,1,1,0) }
sub load_diag_dtD   { my $fn = shift; load_vector($fn,0,1,@_,1,1,0) }
sub load_vec_sb     { my $fn = shift; load_vector($fn,1,0,@_,0,0,0) }
sub load_vec_st     { my $fn = shift; load_vector($fn,0,0,@_,0,0,0) }
sub load_vec_db     { my $fn = shift; load_vector($fn,1,0,@_,1,0,0) }
sub load_vec_dt     { my $fn = shift; load_vector($fn,0,0,@_,1,0,0) }
sub load_vec_sbD    { my $fn = shift; load_vector($fn,1,1,@_,0,0,0) }
sub load_vec_stD    { my $fn = shift; load_vector($fn,0,1,@_,0,0,0) }
sub load_vec_dbD    { my $fn = shift; load_vector($fn,1,1,@_,1,0,0) }
sub load_vec_dtD    { my $fn = shift; load_vector($fn,0,1,@_,1,0,0) }
sub load_vec_sb_tr  { my $fn = shift; load_vector($fn,1,0,@_,0,0,1) }
sub load_vec_st_tr  { my $fn = shift; load_vector($fn,0,0,@_,0,0,1) }
sub load_vec_db_tr  { my $fn = shift; load_vector($fn,1,0,@_,1,0,1) }
sub load_vec_dt_tr  { my $fn = shift; load_vector($fn,0,0,@_,1,0,1) }
sub load_vec_sbd_tr { my $fn = shift; load_vector($fn,1,1,@_,0,0,1) }
sub load_vec_std_tr { my $fn = shift; load_vector($fn,0,1,@_,0,0,1) }
sub load_vec_dbd_tr { my $fn = shift; load_vector($fn,1,1,@_,1,0,1) }
sub load_vec_dtd_tr { my $fn = shift; load_vector($fn,0,1,@_,1,0,1) }


sub pad_to_4 ($) { pad_to(@_, 4) }
sub pad_to_8 ($) { pad_to(@_, 8) }
sub pad_to { $_[0] % $_[1] ? ($_[0] + $_[1] - ($_[0] % $_[1])) : $_[0] }

sub load_matrix {
	my %funcs = &get_funcs_r;
	my ($getN, $getf) = @funcs{qw/N f/};
	my %p = &getOptions;
	my ($dense,$transpose,$matlab) = @_;
	my $class = $dense ? 'DMatrix' : 'SMatrix';
	my ($maxr,$maxc) = map $p{$_}, qw/maxr maxc/;

	if ($matlab) {
		my ($get, $getS, $getI, $getD, $seek, $tell, $eof) =
			@funcs{qw/1 S I D seek tell eof/};
		my $header = $get->(124);
		my $version = $getS->();
		my $endian = $getS->();
		my $skip_to;
		while (!$eof->()) {
			my ($data_type, $size) = map &$getI, 1, 2;
			my $mxsize = $size;
			$skip_to = $size + $tell->();
			warn "Skip $data_type($size)\n" and next if $data_type!=$miMATRIX;
			($data_type, $size) = map &$getI, 1, 2;
			warn "$data_type != $miUINT32\n" and next if $data_type!=$miUINT32;
			my ($flagclass, $nonzero) = map &$getI, 1..$size/4;
			my $mclass = $flagclass & 255;
			($data_type, $size) = map &$getI, 1, 2;
			warn "$data_type != $miINT32\n" and next if $data_type!=$miINT32;
			my ($r, $c) = map &$getI, 1, 2;
			if ($mclass != $mxSPARSE_CLASS and $mclass != $mxDOUBLE_CLASS) {
				warn "Wrong MATLAB class: $mclass\n";
				next;
			}
			$data_type = $getI->();
			my $name;
			if ($data_type!=$miINT8) {
				($data_type, $size) = unpack 'nn', pack 'N', $data_type;
				warn "$data_type != $miINT8\n" and next if $data_type!=$miINT8;
				$name = $get->($size);
				$get->(pad_to_4($size) - $size);
			} else {
				$size = $getI->();
				$name = $get->($size);
				$get->(pad_to_8($size) - $size);
			}
			if ($p{name} and $name ne $p{name}) {
				warn "Skipping $name (Looking for $p{name})\n";
				next;
			}
			my $m = $class->new($transpose?(r=>$c,c=>$r):(r=>$r,c=>$c));
			$m->{name} = $name;
			if ($mclass == $mxSPARSE_CLASS) {
				warn "Reading $m from MATLAB sparse ($nonzero non-zero)\n";
			} else {
				warn "Reading $m from MATLAB dense\n";
			}

			my %fetchers = (
				$miINT32 => $getI,
				$miUINT32 => $getI,
				$miDOUBLE => $getD,
			);
			my %sizes = (
				$miINT32 => 4,
				$miUINT32 => 4,
				$miDOUBLE => 8,
			);
#			$seek = prepend($seek, sub { warn "SEEK @_\n"; });
			if ($mclass == $mxSPARSE_CLASS) {
				($data_type, $size) = map &$getI, 1, 2;
				warn "$data_type!=$miINT32\n" and next if $data_type!=$miINT32;
				my $ir_off = $tell->();
				$seek->($ir_off + pad_to_8 $size);
				($data_type, $size) = map &$getI, 1, 2;
				warn "$data_type!=$miINT32\n" and next if $data_type!=$miINT32;
				my $jc_off = $tell->();
				$seek->($jc_off + pad_to_8 $size);
				($data_type, $size) = map &$getI, 1, 2;
				my $pr_off = $tell->();
				my $fetcher = $fetchers{$data_type};
				warn "No fetcher for $data_type\n" and next if !$fetcher;
				my $sizeof = $sizes{$data_type};
				for my $col (0..$c-1) {
					next if $maxc and $col >= $maxc;
					$seek->($jc_off + 4 * $col);
					my ($jc_start, $jc_end) = map &$getI, 1, 2;
					for my $ind ($jc_start..$jc_end-1) {
						$seek->($ir_off + 4 * $ind);
						my $row = &$getI;
						next if $maxr and $row >= $maxr;
						$seek->($pr_off + 8 * $ind);
						my $val = &$fetcher;
#						warn "[$row,$col]->[$val]\n";
						$m->val($transpose?($col,$row):($row,$col),$val);
					}	
				}
				return $m;
			} else {
				($data_type, $size) = map &$getI, 1, 2;
				my $fetcher = $fetchers{$data_type};
				warn "No fetcher for $data_type\n" and next if !$fetcher;
				my $sizeof = $sizes{$data_type};
				for my $col (0..$c-1) {
					for my $row (0..$r-1) {
						next if $maxc and $col >= $maxc;
						next if $maxr and $row >= $maxr;
						my $val = &$fetcher;
						$m->val($transpose?($col,$row):($row,$col),$val);
					}
				}
				return $m;
			}
		} continue {
			$seek->($skip_to);
		}
		die;
	}

	my $r = $getN->();
	my $c = $getN->();
#	warn "READING($r,$c) v.[$maxr,$maxc]\n";
	$r = min $r, $maxr if $maxr;
	$c = min $c, $maxc if $maxc;
	print STDERR "Reading ($r x $c)".($transpose?"^T":"")." $class".($dense?"\n":"");
	my $m = $class->new($transpose?(r=>$c,c=>$r):(r=>$r,c=>$c));
	if (!$dense) {
		my $nz = $getN->();
		warn " with $nz non-zero entries\n";
		my $col = 0;
		while ($nz>0) {
			my $cnonz = $getN->();
			while ($cnonz-->0) {
				$nz--;
				my $row = $getN->();
			   	my $val = $getf->();
				next if $maxr and $row + 1 >= $maxr;
				next if $maxc and $col + 1 >= $maxc;
			   	$m->val($transpose?($col,$row):($row,$col),$val);
			}
		    $col++;
		}
	} else {
		for my $row (0..$r-1) {
			for my $col (0..$c-1) {
    	        my $val = $getf->();
				next if $maxr and $row + 1 >= $maxr;
				next if $maxc and $col + 1 >= $maxc;
				$m->val($transpose?($col,$row):($row,$col),$val);
			}
		}
	}
    $m;
}

sub load_vector {
	my %funcs = &get_funcs_r;
	my ($getN, $getf) = map $funcs{$_}, qw/N f/;
	my %p = &getOptions;
	my ($dense,$diag,$transpose) = @_;
	my $n = $getN->();
	my $class = $diag ? 'DiagMatrix' : 'Vector';
	my $m = $class->new(r=>($diag?$n:1),c=>$n);
	my $r = 0;
	my $c = -1;
	my $nz = $dense ? $n : $getN->();
	if ($diag) {
		warn "Reading ($n x $n) $class".($dense?"":" ($nz entries)")."\n";
	} else {
		warn "Reading ($n) $class".($dense?"":" ($nz entries)")."\n";
	}
	while ($nz-- > 0) {
		$c = $dense ? $c+1 : $getN->();
		$r = $c if $diag;
		my $v = $getf->();
		$m->val($r,$c,$v);
#		warn "load_vec($r,$c,$v) = ".$m->val($r,$c)."\n";
	}
	$m;
}

sub save_sb      { my $fn = shift; save_sparse($fn,1,0,@_) }
sub save_st      { my $fn = shift; save_sparse($fn,0,0,@_) }
sub save_db      { my $fn = shift;  save_dense($fn,1,0,@_) }
sub save_dt      { my $fn = shift;  save_dense($fn,0,0,@_) }
sub save_sbD     { my $fn = shift; save_sparse($fn,1,1,@_) }
sub save_stD     { my $fn = shift; save_sparse($fn,0,1,@_) }
sub save_dbD     { my $fn = shift;  save_dense($fn,1,1,@_) }
sub save_dtD     { my $fn = shift;  save_dense($fn,0,1,@_) }
sub save_matlabs { my $fn = shift; save_matlab($fn,1,1,1,@_) }
sub save_matlabd { my $fn = shift; save_matlab($fn,1,1,0,@_) }

#	INIT {
#		my $st = \%::;
#		$st = $st->{$_."::"} for split /::/, __PACKAGE__;
#		for my $f (grep /^(?:save|load)_/, keys %$st) {
#			no warnings 'redefine';
#			no strict 'refs';
#			my $orig = *{$st->{$f}}{CODE};
#			*$f = prepend($orig, sub { warn "$f(@_)\n"; });
#		}
#	}

sub save_sparse {
	my %funcs = &get_funcs_w;
	my ($putN, $putf, $putSpace, $putCR) = map $funcs{$_}, qw/N f SPC CR/;
	my ($m) = @_;
	warn "Write sparsing $m\n";
	my $nz = 0;
	for my $r ($m->nzr) {
		for my $c ($m->nzc($r)) {
			$nz++ if $m->val($r,$c);
		}
	}
	$putN->($m->r);
	&$putSpace;
	$putN->($m->c);
	&$putSpace;
	$putN->($nz);
	&$putCR;
	for my $c (0..$m->c-1) {
		my @nzr = sort { $a <=> $b } $m->nzr($c);
		my @val = map $m->val($_,$c), @nzr;
		my @act_nz = grep $val[$_], 0..$#val;
		@nzr = @nzr[@act_nz];
		@val = @val[@act_nz];
		$putN->(0+@nzr);
		&$putCR;
		for my $r (@nzr) {
			$putN->($r);
			&$putSpace;
			$putf->($m->val($r,$c));
			&$putCR;
		}
	}
}
sub save_dense {
#use Data::Dumper; print Dumper \@_;
	my %funcs = &get_funcs_w;
	my ($putN, $putf, $putSpace, $putCR) = map $funcs{$_}, qw/N f SPC CR/;
	my ($m) = @_;
	$putN->($m->r);
	&$putSpace;
	$putN->($m->c);
	&$putCR;
	for my $r (0..$m->r-1) {
		for my $c (0..$m->c-1) {
			&$putSpace if $c;
			$putf->($m->val($r,$c));
#			warn "WRiting [$r,$c] = ".$m->val($r,$c)."\n";
		}
		&$putCR;
	}
}
use POSIX qw/strftime/;

sub save_matlab {
	my %funcs = &get_funcs_w;
	my ($putI, $putS, $putD, $put) = map $funcs{$_}, qw/I S D bytes/;
	my $sparse = shift;
	my %p = &getOptions;
	my ($m) = @_;
	my $dtemp = $p{date}||"%F\@%T";
	my $tag = pack "A*", strftime "MATLAB 5.0 MAT-file Platform: GLNX86 Date: $dtemp ".($sparse?"sparse":"dense")." matrix output by ".__PACKAGE__, localtime;
	$tag .= " " x (124-length $tag);
	$tag = substr $tag, 0, 124;
	$put->($tag);
	$putS->(256);
	$putS->(unpack 'n', 'MI'); # endianness marker

	my $name = $p{name} || $m->{name} || 'noname';
	$name = pack "C*", unpack "C*", $name;
	my $name_act_len = length $name;
	$name .= "\0" x (8 - $name_act_len % 8) if $name_act_len % 8;

### matrix data element header
	$putI->($miMATRIX);
	my $total_size = 0;
	$total_size += 16; # array flags
	$total_size += 16; # dimensions
	$total_size += 8 + length $name; # name

	my $total_nz = 0;
	if ($sparse) {
		for my $r ($m->nzr) {
			for my $c ($m->nzc($r)) {
				$total_nz++ if $m->val($r,$c);
			}
		}
		$total_size += 8 + $total_nz * 8; # ir
		$total_size += 16 + $total_nz * 8; # jc
		$total_size += 8 + $total_nz * 8; # data values
	} else {
		$total_size += 8 + 8 * $m->r * $m->c; # data values
	}
	warn "Writing $m as MATLAB ".($sparse?"sparse ($total_nz non-zero)":"dense")."\n";
#	$total_size = calc();
	$putI->($total_size);

### array flags
	$putI->($miUINT32);
	$putI->(8);
	$putI->($sparse ? $mxSPARSE_CLASS : $mxDOUBLE_CLASS);
	$putI->($total_nz);

### dimensions
	$putI->($miINT32);
	$putI->(8);
	$putI->($m->r);
	$putI->($m->c);

### array name
	$putI->($miINT8);
	$putI->($name_act_len);
	$put->($name);

	if (!$sparse) {
		my $tot = $m->r * $m->c;
		$putI->($miDOUBLE);
		$putI->($tot * 8);
		for my $c (0..$m->c-1) {
			for my $r (0..$m->r-1) {
				$putD->($m->val($r,$c));
			}
		}
	} else {
		$putI->($miINT32);
		$putI->($total_nz * 4);
		for my $c (sort { $a <=> $b } $m->nzc) {
			for my $r (sort { $a <=> $b } $m->nzr($c)) {
				$putI->($r) if $m->val($r,$c);
			}
		}
		$putI->(0) if $total_nz % 2;
		$putI->($miINT32);
		$putI->(($m->c + 1) * 4);
		my $so_far = 0;
		for my $c (0..$m->c-1) {
			$putI->($so_far);
			for my $r (sort { $a <=> $b } $m->nzr($c)) {
				$so_far++ if $m->val($r,$c);
			}
		}
		$putI->($so_far);
		$putI->(0) if not $m->c % 2;
		$putI->($miDOUBLE);
		$putI->($total_nz * 8);
		for my $c (sort { $a <=> $b } $m->nzc) {
			for my $r (sort { $a <=> $b } $m->nzr($c)) {
				my $v = $m->val($r,$c);
				next if !$v;
				$putD->($v);
			}
		}
	}
}

package Matrix;
use overload '""' => \&stringy;
sub stringy { my @d = map $_[0]->$_, qw/r c/; (shift->class)."($d[0] x $d[1])" }
sub r { $_[0]{r} } sub rows { &r }
sub c { $_[0]{c} } sub cols { &c }
sub dim { my $h = shift; ($h->{r}, $h->{c}) }
sub rowvec {
	my ($self,$n) = @_;
	my $v = Vector->new(n=>$self->c);
	$v->val(0,$_,$self->val($n,$_)) for $self->nzc($n);
	$v;
}
sub colvec {
	my ($self,$n) = @_;
	my $v = Vector->new(n=>$self->r);
	$v->val(0,$_,$self->val($_,$n)) for $self->nzr($n);
	$v;
}
sub inc {
	my ($self,$r,$c,$add) = @_;
	my $v = $self->val($r,$c);
	$self->val($r,$c,$v+$add);
}
sub clone {
	my ($self,$transpose) = @_;
	my $class = ref $self;
	my $r = $self->r;
	my $c = $self->c;
	my $new = $class->new($transpose?(r=>$c,c=>$r):(r=>$r,c=>$c));
	for $r ($self->nzr) {
		for $c ($self->nzc($r)) {
			$new->val($transpose?($c,$r):($r,$c),$self->val($r,$c));
		}
	}
	$new;
}
sub t { shift->clone(1) }
*tr = \&t;
*transpose = \&t;
sub mult {
	my ($A, $B) = @_;
	my (@cl) = map $_->class, @_;
	my (@sp) = map $_->sparseness, @_;
	my ($minsp,$maxsp) = sort { $a <=> $b } @sp;
	my $d = $maxsp-$minsp;
	@cl = map $cl[$_], sort { $sp[$a] <=> $sp[$b] } 0..$#cl;
	my $cP = $d > 1 ? shift @cl : pop @cl;
	my ($Ar, $Ac, $Br, $Bc) = map $_->dim, @_;
	die "Mismatched multiplication $A by $B\n" if $Ac != $Br;
	my $p = $cP->new(r=>$Ar,c=>$Bc);
	warn "Mult @_ -> $p\n";
	#my ($iA,$iB,$vA,$vB) = ($sp[0] >= $sp[1]) ? ($A,$B) : ($B,$A);
	for my $r ($A->nzr) {
		for my $i ($A->nzc($r)) {
			my $av = $A->val($r,$i);
			for my $c ($B->nzc($i)) {
				$p->inc($r,$c,$av*$B->val($i,$c));
			}
		}
	}
	$p;
}
sub rmult { &mult }
sub lmult { $_[1]->mult($_[0]) }
sub class { ref shift }
sub sparseness { return 0 }
package SMatrix;
our @ISA = qw/Matrix/;
sub sparseness { return 1 }
sub new {
	my $class = shift;
	my $self = bless { nzind => {}, data => {}, @_ }, $class;
}
sub nzr {
	my $self = shift;
	my $data = $self->{data};
	return (keys %$data) unless @_;
	my $c = shift;
	my $nzi = $self->{nzind};
	return () unless exists $nzi->{$c};
	keys %{$nzi->{$c}};
}
sub nzc {
	my $self = shift;
	my $nzi = $self->{nzind};
	return (keys %$nzi) unless @_;
	my $r = shift;
	my $data = $self->{data};
	return () unless exists $data->{$r};
	keys %{$data->{$r}};
}
sub val {
	my ($self,$r,$c,$v) = @_;
#	warn __PACKAGE__."::val($r,$c".(defined($v)?",$v":"").")\n";
	my $d = $self->{data};
	if (defined $v) {
		$self->{nzind}{$c}{$r}=1;
		$d->{$r}{$c} = $v;
	} else {
		return 0 unless exists $d->{$r};
		return 0 unless exists $d->{$r}{$c};
		$d->{$r}{$c};
	}
}
package DiagMatrix;
our @ISA = qw/SMatrix/;
sub sparseness { return 2 }
sub new {
	my $class = shift;
	my (%opt) = @_;
	my $n = $opt{n} || $opt{r} || $opt{c};
	SMatrix::new($class,r=>$n,c=>$n);
}
sub inv {
	my $self = shift;
	return if grep !$_, map $self->val($_,$_), 0..$self->r-1;
	my $inv = $self->clone;
	$inv->val($_,$_,1/$self->val($_,$_)) for $self->nzr;
	$inv;
}
package Vector;
our @ISA = qw/SMatrix/;
sub new {
	my $class = shift;
	my (%opt) = @_;
	my $n = $opt{n} || $opt{c};
	SMatrix::new($class,r=>1,c=>$n);
}
sub inv { die "Can't invert a vector\n"; }
sub mag {
	my $self = shift;
	my $sum = 0;
	$sum += ($self->val(0,$_))**2 for $self->nzc(0);
	sqrt $sum;
}
sub init {
	my ($self, %h) = @_;
	$self->val(0,$_,$h{$_}) for keys %h;
	$self;
}
sub dot {
	my ($self, $o) = @_;
	my ($tall) = ($o->r > 1) ? 1 : 0;
	my $sum = 0;
	if ($tall) {
		$sum += $self->val(0,$_) * $o->val($_,0) for $self->nzc;
	} else {
		$sum += $self->val(0,$_) * $o->val(0,$_) for $self->nzc;
	}
	$sum;
}
sub mult_vm { shift->multby(@_,0) }
sub mult_mv { shift->multby(@_,1) }
sub multby {
	my ($self, $m, $vec_r) = @_;
	my ($N,$iter) = $vec_r ? ($m->dim) : reverse($m->dim);
	die "Mismatch N=$N, but @{[$vec_r ? ($m, $self) : ($self, $m)]}\n" if $iter != $self->c;
	my $out = Vector->new(c=>$N);
	for my $r (0..$N-1) {
		for my $c (0..$iter-1) {
			$out->inc(0,$r,$self->val(0,$c)*$m->val($vec_r?($r,$c):($c,$r)));
		}
	}
	$out;
}
package DMatrix;
our @ISA = qw/Matrix/;
sub new {
	my $class = shift;
	my $self = bless { @_ }, $class;
	use Data::Dumper; die "Bad input to DMatrix->new(".Dumper(\@_).")" if grep !defined, $self->{r}, $self->{c};
	$self->{data} = [(0) x ($self->{r} * $self->{c})];
	$self;
}
sub nzr { return 0..shift->{r}-1; }
sub nzc { return 0..shift->{c}-1; }
sub val {
	my ($self,$r,$c,$v) = @_;
#	warn __PACKAGE__."::val($r,$c".(defined($v)?",$v":"").")\n";
	my $d = $self->{data};
	my $i = ($r*$self->{c})+$c;
	return $d->[$i] unless defined $v;
	$d->[$i] = $v;
}
1;
