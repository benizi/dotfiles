package M;
use strict;
use warnings;
sub symtab ($) { my $st = \%::; $st = $st->{$_."::"} for split /::/, shift; $st}
my %before; BEGIN { $before{$_}++ for keys %{symtab __PACKAGE__}; }
use Data::Dumper;
use URI::Escape qw/uri_unescape uri_escape/;
use Digest::MD5 qw/md5 md5_hex md5_base64/;
use Digest::SHA1 qw/sha1 sha1_hex sha1_base64/;
use MIME::Base64 qw/encode_base64 decode_base64/;
use File::Basename;
use File::Find;
use Storable qw/nstore retrieve/;
eval "use MyMatrices; 1" and MyMatrices->import;
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
*md5file = _filesum("Digest::MD5");
*sha1file = _filesum("Digest::SHA1");
*base = _underscored(\&basename);
my %not;
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
		/^v(?:erb(?:ose)?)?$/ ? ($verbose = 1) :
	0 } @_;
	my $st = symtab __PACKAGE__;
	my $out = symtab caller;
	my @x = @_ ? (@_) : grep !/$_not_re/&&!$not{$_}&&!$before{$_}, keys %$st;
	$verbose and warn "Exporting: (@x)\n";
	for my $fn (@x) {
		for ($fn, $alias{$fn} ? @{$alias{$fn}} : ()) {
			$out->{$_} = $st->{$fn};
		}
	}
}
1;
