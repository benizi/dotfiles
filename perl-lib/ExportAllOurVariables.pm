package ExportAllOurVariables;

my %symtab_before;
sub _symtab {
	my $package = shift || __PACKAGE__;
	my $symtab = \%::;
	$symtab = $$symtab{$_.'::'} for split /::/, $package;
	$symtab;
}

my $exclude;
BEGIN { $exclude = qr/^(?:[^a-z]|import$)/; }
sub export_all { _export_all(0, @_) }
sub export_up { _export_all(1, @_) }
sub _export_all {
	my $level = shift;
	my ($package, $to) = map {
		(@_ and $_[0] !~ /^[&\$\@\%]/)
		? shift
		: (caller($_))[0];
	} map $level + $_, 1, 2;
#	warn "_EXPORT_ALL level=$level pack=$package to=$to\n";
	my @extra = @_;
	my $oldsym = $symtab_before{$package} || {};
	my $newsym = _symtab $package;
	my $tosym = _symtab $to;
	my @ex_syms = grep !/$exclude/,
		($level and @_)
		? @_
		: grep !$$oldsym{$_}, keys %$newsym;
	push @ex_syms, @extra;
	my %sigil = qw{SCALAR $ CODE & ARRAY @ HASH %};
	my %r_sig = reverse %sigil;
	sym: for my $ex (@ex_syms) {
		(my $nosig = $ex) =~ s{^([&\$\@\%])}{};
		for my $s (($nosig ne $ex) ? ($r_sig{$1}) : keys %sigil) {
			next unless defined *{$$newsym{$nosig}}{$s};
#			warn "Would export $sigil{$s}$nosig from $package to $to\n";
			$$tosym{$nosig} = $$newsym{$nosig};
			next sym;
		}
		die "$ex is not exported by $package";
	}
}

sub import {
	my $selfpack = shift;
	my $selftab = _symtab $selfpack;
	my $package = shift || caller;
	my $symtab = _symtab $package;
	for my $exp (qw/export_all export_up/) {
		warn "Overwriting $package\'s $exp\n" if exists $$symtab{$exp};
		$$symtab{$exp} = $$selftab{$exp};
	}
	$symtab_before{$package}{$_} = 1 for keys %$symtab;
}

1;
