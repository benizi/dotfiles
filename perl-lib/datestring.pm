package datestring;
use strict;
use warnings;
use 5.006;
our $VERSION = '0.01';

use overload
	'+' => 'date_add',
	'-' => 'date_sub',
	'""' => 'date_string',
	;

require Date::Manip;

sub unimport {
	$^H{datestring} = undef;
	overload::remove_constant('q' => '');
}

sub import {
	my $self = shift;
	$^H{datestring} = 1;
	overload::constant( q => \&string_to_date );
}

sub string_to_date {
	my ($input, $processed, $type) = @_;
	# warn "Input: {$input} Processed: {$processed} Type: {$type}\n";
	return $processed if $type eq 'tr';
	local $_ = $processed;
	if (s/^d(?:ate)?://) {
		return bless \do{my $val = Date::Manip::ParseDate($_)}, __PACKAGE__;
	} elsif (s/^((?:de(?:l(?:ta)?)?|D|[+\-]))://) {
		$_ = "$1 $_" if $1 eq '+' or $1 eq '-';
		return bless \do{my $val = Date::Manip::ParseDateDelta($_)}, __PACKAGE__;
	}
	$_
}

sub negate {
	my $d = shift;
	$$d =~ s/^\+/-/ or $$d =~ s/^-/+/;
	$d;
}

sub upgrade {
	local $_ = @_ ? shift : $_;
	return $_ if ref;
	my $v = undef;
	eval { $v = Date::Manip::ParseDateDelta($_) };
	if (not defined $v) {
		eval { $v = Date::Manip::ParseDate($_) };
	}
	bless \do{my $o = $v}, __PACKAGE__;
}

sub date_sub {
	my ($A, $B, $reversed) = @_;
	$_ = upgrade for $A, $B;
	$reversed and ($A, $B) = ($B, $A);
	$$B =~ /^[+\-]/
	? date_add($A, negate($B), 0)
	: negate(date_add($A, $B, 0));
}

sub date_add {
	my ($A, $B, $reversed) = @_;
	$_ = upgrade for $A, $B;
	bless \do{my $val = Date::Manip::DateCalc($$A, $$B, 1)}, __PACKAGE__;
}

sub date_string {
	my $d = shift;
	return Date::Manip::UnixDate($$d, "%Y-%m-%d %H:%M:%S") unless $$d =~ /^[+\-]/;
	# Xv = value of X
	# Xd = X and smaller
	# Xh = X and larger
	# Xt = total as X
	my $first_pass = Date::Manip::Delta_Format($$d, approx => 0, '%wt');
	my $fmt = abs $first_pass > 5 ? "%yhy %MvM %wvw %dvd %hdh" : "%whw %dvd %hvh %mvm %svs";
	Date::Manip::Delta_Format $$d, approx => 2, $fmt;
}

1;
