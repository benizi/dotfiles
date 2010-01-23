package M;
use strict;
use warnings;
sub symtab ($) { my $t = \%::; $t = $$t{$_."::"} for split /::/, shift; $t }
my %before; BEGIN { %before = %{symtab __PACKAGE__}; delete $before{symtab}; }
my %not;
sub _underscored {
	my $sub = shift;
	return sub {
		@_ or unshift @_, $_;
		&$sub;
	};
}
sub _fn_or_fh {
	my $sub = shift;
	my $mode = @_ ? (shift) : '<';
	return sub {
		my $fnh = shift;
		my $fh;
		if (ref $fnh) { $fh = $fnh }
		elsif ($mode !~ /</ or -r $fnh) { open $fh, $mode, $fnh }
		unshift @_, $fh;
		&$sub;
	};
}
sub _filesum {
	my $dig = shift;
	return _underscored(_fn_or_fh(sub {
		my $fh = shift;
		my $obj = $dig->new;
		$obj->addfile($fh);
		$obj->hexdigest;
	}));
}
sub _read_file {
	return _underscored(_fn_or_fh(sub {
		my $fh = shift;
		$fh or return '';
		my $ret = do { undef local $/; <$fh> };
		close $fh;
		$ret;
	}));
}
sub mkdir_p {
	my $d = shift;
	return 1 if -d $d;
	my @p = File::Spec->splitdir($d);
	for (0..$#p) {
		my $dir = File::Spec->catdir(@p[0..$_]);
		next if -d $dir;
		mkdir $dir or return 0;
	}
	1;
}
sub _shellquote {
	local $_ = shift;
	s{([\\\'\`\"\$\ \[\]\*\?])}{\\$1}g;
	$_;
}
*shell_quote = _underscored(\&_shellquote);
*quote_shell = \&shellquote;
sub run_cmd {
	if (grep ref, @_) {
		&IPC::Run::run;
	} else {
		my @cmd = @_;
		my $ret = system { $cmd[0] } @cmd;
		!$ret;
	}
}
sub map2bits {
	my ($txt, $bits, $nouse, $all)=@_;
	$nouse = '' unless defined $nouse;
	$nouse .= "\\s~\\-" unless $all;
	my $maxV = (2**$bits)-1;
	my @charmap = map [], 0..$maxV;
	push @{$charmap[ord()&$maxV]}, $_ for
		grep !/[$nouse]/,
		grep /[[:print:]]/,
		map chr, 0..0x7f;
	my @no = grep !@{$charmap[$_]}, 0..$maxV;
	local $_ = $txt;
	$_ .= "0" while length()%$bits;
	my $r = '';
	for(grep $_, split/(.{$bits})/) {
		my $val = (eval"0b$_")&$maxV;
		my $c = $charmap[$val];
		die "Can't represent [$val] {bits=$bits, max=$maxV} (or @no)\n" if !@$c;
		$r .= $$c[0];
		push @$c, shift @$c
	}
	$r;
}
sub _xml_file { XML::Twig->new->parsefile(shift) }
*xml_file = _underscored(\&_xml_file);
my %in_module;
sub my_use {
	my ($mod, @args) = @_;
	my (@exc, @underscore);
	s/_$// and push @underscore, $_ for @args;
	@args = grep { not
		ref() ? (push @exc, $_) :
	0 } @args;
	my $fail_ok = 0;
	my $warn = 1;
	if ('optional' eq lc $mod) {
		$warn = 0 if $mod ne lc $mod;
		$mod = shift @args;
		$fail_ok = 1;
	}
	my %before_loc = %{symtab __PACKAGE__};
	if (eval "use $mod (); 1") {
		$mod->import(@args);
		my %new = %{symtab __PACKAGE__};
		delete $new{$_} for keys %before_loc;
		for my $no (@exc) {
			$not{$_}++ for grep /$no/, keys %new;
		}
		for my $und (@underscore) {
			my $old = "__".$und;
			no strict 'refs';
			no warnings 'redefine';
			no warnings 'prototype';
			*$old = \&$und;
			*$und = _underscored(\&$old);
		}
		$in_module{$_} = $mod for grep !$not{$_}, keys %new;
	} else {
		die "$@" unless $fail_ok;
		warn "Couldn't load $mod\n" if $warn;
	}
}
sub optuse { my_use optional => @_; }
sub OPTION { my_use OPTIONAL => @_; }
my_use 'Data::Dumper';
optuse 'URI::Escape', qw/uri_unescape_ uri_escape_/;
my_use 'Digest::MD5', qw/md5_ md5_hex_ md5_base64_/;
optuse 'Digest::SHA1', qw/sha1_ sha1_hex_ sha1_base64_/;
my_use 'MIME::Base64', qw/decode_base64_/;
*encode_base64 = sub {
	local $_ = MIME::Base64::encode_base64(@_?$_[0]:$_);
	s/\n\Z//;
	$_
};
my_use 'MIME::QuotedPrint', qw/decode_qp_/;
*encode_qp = sub {
	local $_ = MIME::QuotedPrint::encode_qp(@_?$_[0]:$_);
	s/\n\Z//;
	$_
};
my_use 'File::Basename';
my_use 'File::Find';
my_use 'Storable', qw/nstore retrieve_/;
my_use 'List::Util', qw/min max/;
optuse 'Date::Manip', qr/^Date_/;
optuse 'XML::Twig';
OPTION 'MyMatrices';
OPTION 'Acme::MetaSyntactic', 'batman';
OPTION 'Statfs';
my_use 'POSIX', 'strftime';
my_use 'IPC::Run';
*hms = _underscored(sub {
	my $tot = shift;
	my $neg = ($tot < 0) ? '-' : '';
	$tot *= -1 if $neg;
	$tot = int $tot if $tot > 1;
	my @div = ([d=>0],[h=>24],[m=>60],[s=>60]);
	my @n;
	my @l;
	while ($_ = pop @div) {
		last if @n and not $tot;
		my ($l, $m) = @$_;
		my $rem = $m ? $tot % $m : $tot;
		unshift @n, $rem;
		unshift @l, $l;
		last if not $m;
		$tot -= $rem;
		$tot /= $m;
	}
	$neg . join ':', map sprintf("%0*d%s", $_?2:1, $n[$_], $l[$_]), 0..$#n;
});
*md5file = _filesum("Digest::MD5");
*sha1file = _filesum("Digest::SHA1");
*read_file = _read_file;
*base = _underscored(\&basename);
*dir = _underscored(\&dirname);
BEGIN { $not{$_}++ for qw/BEGIN import before/; }
my $_not_re = qr/^_/;
my %alias;
for (qw/uri_unescape:unuri uri_escape:uri
	encode_base64:base64_encode decode_base64:base64_decode/) {
	my ($fn, @al) = split /:/;
	push @{$alias{$fn}}, @al;
}
sub import {
	shift;
	my $verbose = 0;
	my $help = 0;
	@_ = grep { not
		/^v(?:erb(?:ose)?)?$/ ? (++$verbose) :
		/^h(?:elp?)?((?:die)?)$/ || /^ind(?:ex)?$/ ? ($help=$1?2:1) :
	0 } @_;
	my $st = symtab __PACKAGE__;
	my $new;
	$$new{$_}=$$st{$_} for grep !/$_not_re/&&!$not{$_}&&!$before{$_}, keys %$st;
	for my $u (grep /_/, keys %$new) {
		(my $n = $u) =~ tr/_//d;
		$$new{$n} = $$new{$u};
	}
	my $out = symtab caller;
	my @x = @_ ? (@_) : (keys %$new);
	$verbose and warn "Exporting: (@x)\n";
	my %help;
	for my $fn (@x) {
		die "$fn is not exported by ".__PACKAGE__."\n" if !$$new{$fn};
		my @also;
		@also = @{$alias{$fn}} if $alias{$fn};
		my @as;
		my %seen;
		for ($fn, @also) {
			my $s = ''.$_;
			do {
				push @as, $s unless $seen{$s}++;
			} while $s =~ tr/_//d;
		}
		if ($help) {
			push @{$help{"From ".($in_module{$fn}||$INC{__PACKAGE__.".pm"})}},
				[ @as ];
		}
		$$out{$_} = $$new{$fn} for @as;
	}
	for my $p (sort keys %help) {
		warn "$p\n";
		my @f = sort { $$a[0] cmp $$b[0] } @{$help{$p}};
		warn "$$_[0]", (@$_ > 1) ? " [also as: @$_[1..$#$_]]" : "", "\n" for @f;
	}
	exit if $help > 1;
}
1;
