package QGram;
our @EXPORT = qw/&qgram &qgrams/;
our @EXPORT_OK;
my ($N, $maxN); BEGIN { $N=3; $maxN=6; }
BEGIN { @EXPORT_OK = map "&qgram$_", map { ($_, "l$_") } '', 1..$maxN; }
use base qw/Exporter/;
use List::Util qw/min/;
for my $l ('', 's', 'l', 'ls') {
	eval qq<sub qgram$l$_ { unshift \@_, $_; &qgram$l; }\n> for 1..$maxN;
}
sub qgraml {
	my ($q, $s) = @_;
	$s = ("\1" x ($q-1)) . $s . ("\2" x ($q-1));
	map substr($s, $_, $q), 0..length($s)-$q;
}
sub qgramls {
	my ($q, $s) = @_;
	$s = ("\1" x ($q-1)) . $s . ("\2" x ($q-1));
	map join('',sort split //, substr($s,$_,$q)), 0..length($s)-$q;
}
sub qgram { _qgram(\&qgraml, $N, @_); }
sub qgrams { _qgram(\&qgramls, $N, @_); }
sub _qgram {
	my ($f, $q, $s, $t) = @_;
	my @c;
	for ($s, $t) {
		my $h = {};
		$h->{$_}++ for $f->($q,$_);
		push @c, $h;
	}
	my $tot = 0;
	$tot += $_ for map values %$_, @c;
	my $com = 0;
	my %seen;
	for my $k (grep !$seen{$_}++, map keys %$_, @c) {
		$com += @c * min map $_->{$k}||0, @c;
	}
	$com/$tot;
}
if (!caller) {
	warn "Running test. Enter test pairs separated by a tab.\n";
	while (<>) {
#		print;
		chomp;
#		for my $f (\&qgraml3, \&qgramls3) {
		next unless my ($s, $t) = (/^(.*)\t(.*)$/);
		for my $f (
			\&qgraml3,
			\&qgramls3,
			\&qgraml4,
			\&qgramls4,
			) {
			my @c;
			for my $str ($s, $t) {
				my $h = {};
				$h->{$_}++ for $f->($str);
				print join(",",sort map"<$_>",map{s/([\1\2])/"\\".ord($1)/ge; $_}$f->($str)), "\n";
				push @c, $h;
			}
			my $tot = 0;
			$tot += $_ for map values %$_, @c;
			my ($c, $d) = @c[0,1];
			my $com = 0;
			$com += 2 * min $c->{$_}, $d->{$_} for grep $d->{$_}, keys %$c;
			print "$s\t$t\t$com\t$tot\n";
		}
		for (\&qgram3, \&qgrams3) { print "$s\t$t\t",$_->($s,$t),"\n"; }
	}
}
1;
