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
		if (ref $fnh) { $fh = $fnh; } else { open $fh, $mode, $fnh; }
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
sub read_file {
	my $fn = shift;
	open my $fh, '<', $fn or die "<$fn: $!";
	my $ret = do { undef local $/; <$fh> };
	close $fh;
	$ret;
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
	my @cmd = @_;
	my $ret = system { $cmd[0] } @cmd;
	!$ret;
}
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
my_use 'File::Basename';
my_use 'File::Find';
my_use 'Storable', qw/nstore retrieve_/;
my_use 'List::Util', qw/min max/;
optuse 'Date::Manip', qr/^Date_/;
optuse 'XML::Twig';
OPTION 'MyMatrices';
OPTION 'Acme::MetaSyntactic', 'batman';
my_use 'POSIX', 'strftime';
*md5file = _filesum("Digest::MD5");
*sha1file = _filesum("Digest::SHA1");
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
	@_ = grep { not
		/^v(?:erb(?:ose)?)?$/ ? (++$verbose) :
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
	for my $fn (@x) {
		die "$fn is not exported by ".__PACKAGE__."\n" if !$$new{$fn};
		for ($fn, $alias{$fn} ? @{$alias{$fn}} : ()) {
			do {
				$$out{$_} = $$new{$fn};
			} while tr/_//d;
		}
	}
}
1;
