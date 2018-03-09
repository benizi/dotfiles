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

sub new {
	my ($pkg, $kind, $input) = @_;
	my $val;
	if (ref $kind) {
		$val = $kind;
	} else {
		$val = "Date::Manip::$kind"->new;
		return undef if $val->parse($input);
	}
	bless \$val, $pkg;
}

# return 1 if datestring is active (lexically scoped via `use/no datestring;`)
sub active {
	my $level = shift || 0;
	my @caller = caller $level;
	my $H = $caller[10] // {};
	$$H{datestring} // 0;
}

sub string_to_date {
	my ($input, $processed, $type) = @_;
	return $input unless active;
	return $processed if ($type//'') eq 'tr';
	local $_ = $processed;
	if (s/^d(?:ate)?://) {
		return new datestring(Date => $_);
	} elsif (my ($marker) = s/^((?:de(?:l(?:ta)?)?|D|[+\-]))://) {
		$_ = "$marker $_" if $marker =~ /[+\-]/;
		return new datestring(Delta => $_);
	}
	$_
}

sub date_sub {
	shift->date_add(@_, 1);
}

sub date_add {
	my ($A, $B, $reversed, $subtract) = @_;
	($A, $B) = ($B, $A) if $reversed;
	new datestring($$A->calc($$B, $subtract));
}

sub date_string {
	my $d = shift;
	return $$d->printf("%Y-%m-%d %H:%M:%S") if $$d->is_date;
	# Fields are: year Month week day hour minute second
	# Formats:
	# %Av = value of field A
	# %ABC = value of fields B down to C in terms of A
	# e.g., %dyd = number of days in the year + Month + week + day fields
	my @fields;
	# Show large fields if set (or a larger one is set)
	for my $field (qw(y M w d)) {
		push @fields, $field if @fields or $$d->printf(qq(\%${field}v));
	}
	# Always show small fields
	push @fields, qw(h m s);
	join ' ', $$d->printf(map qq(\%${_}v${_}), @fields);
}

1;
