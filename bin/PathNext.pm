package PathNext;
use base 'Exporter';
our @EXPORT = our @EXPORT_OK = qw/&path_next/;
use FindBin '$Bin';
use File::Spec;
use File::Basename;

sub fingerprint { join "\t", (stat shift)[0,1] }

sub path_next {
	my $arg0 = shift;
	my $zero = fingerprint $arg0;
	my $prog = basename $arg0;
	for my $dir (File::Spec->path) {
		my $bin = File::Spec->catfile($dir, $prog);
		next unless -x $bin;
		next if $zero eq fingerprint $bin;
		return $bin;
	}
	die "Couldn't find original $prog in path\n";
}

1;
