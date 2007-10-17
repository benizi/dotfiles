#!/usr/bin/perl
use Getopt::Long qw/:config pass_through/;
GetOptions(
	'debug+' => \(my $debug = 0),
	'emergeopts|eopts=s' => \(my $eopts = '-pNDuv'),
) or die 'options';
my @args = @ARGV;
@args = ('world') unless @args;
@ARGV = glob "/usr/portage/profiles/use.{,local.}desc";
my $dc = qr/[a-z0-9_+]/i;
my $fc = qr/[a-z0-9_+\-.]/i;
my $dir = qr/(?:virtual|$dc+-$dc+)/i;
my %flags;
while (<>) {
	my $local = $ARGV =~ /local/;
	my $lg = $local ? 'local' : 'global';
	my $m = $local
		? qr/^($dir)\/($dc+(?:-$dc+)*):($fc+)\s+-\s/
		: qr/^()()($fc+)\s+-\s/;
	next unless s/$m//;
	chomp;
	my ($group, $build, $flag, $desc) = ($1, $2, $3, $_);
	$flags{$flag}{$group}{$build} = $desc;
}
# use Data::Dumper; print Dumper \%flags and exit if $debug > 1;
my $toemerge = "emerge $eopts @args |";
print "Emerging $toemerge\n";
open my $emerge, $toemerge;
my @blocks;
my @allflags;
my $version = qr/\d+(?:\.\d+)*(?:[a-z]+|_[a-z]+\d+)?(?:-r\d+)?/i;
while (<$emerge>) {
	chomp(my $line = $_);
	push @blocks, $_ and next if /^\[blocks/;
	my ($eflags, $group, $build, $version, $previous, $flags, $size) =
	/^\[ebuild\s(......)\] # ebuild flags
	\s($dir)\/($dc+(?:-$dc+)*?)-($version) # Directory + package + version
	\s((?:\[$version\])?) # optional "upgrading from" clause
#	((?:\s\(?[\-+]$fc+\*?\)?)*) # old USE flags format
	((?:\s[A-Z]+="[^"]+")*) # variables
	\s([\d,]+)\skB # size
	/x;
	$previous =~ tr/\[\]//d;
	if (not defined $eflags) {
		next unless /^\[ebuild/;
		die "Unparsed ebuild line:\n$_";
	}
	my $errormsg = "$line\n\$eflags<$eflags>\n\$group<$group> \$build<$build>\n\$version<$version> \$previous<$previous>\n\$flags\n<$flags>\n\$size<$size>\n";
	print $errormsg if $debug;
	my @VARS = ($flags =~ /\s([A-Z]+="[^"]+")/g);
	die "Couldn't split flags: <$flags>\n" unless @VARS;
	@VARS = map { /^(\w+)="([^"]+)"/ ? [ $1, split ' ', $2 ] : $_ } @VARS;
	die "Unparsed flag: $_\n" for grep !ref, @VARS;
	use Data::Dumper; $debug and print Dumper \@VARS;
	for (@VARS) {
		my ($var, @vals) = @$_;
		if ($var eq 'USE') {
			for (@vals) {
				my $not_avail = (tr/\(\)//d) ? 1 : 0;
				my $changed = (tr/*//d) ? 1 : 0;
				my $newflag = (s/%$//) ? 1 : 0;
				my $add_del = (s/^-//) ? '-' : '+';
				my @path = ($flags{$_}{$group}{$build}) ? ($group, $build) : ('', '');
				push @allflags, [ $_, @path, $add_del, $changed, $newflag, $not_avail ];
			}
		} else {
			$debug and warn "Not parsing $var\n";
		}
	}
}
die "Blockers:\n", @blocks if @blocks;
my %ambiguous;
{my %all;$all{$_->[2]}{$_->[1]}++ for @allflags;$ambiguous{$_}++ for grep 1<keys%{$all{$_}}, keys %all;}
my %seen;
my %color = ( '+' => "\e[31;1m", '-' => "\e[34;1m", '!' => "\e[33m" );
for (grep !$seen{join $;, @$_[0..2]}++, sort {
	$$a[0] cmp $$b[0] or
	$$a[1] cmp $$b[1] or
	$$a[2] cmp $$b[2]
} @allflags) {
	my ($flag, $grp, $bld, $add, $change, $newflag, $na) = @$_;
	my $where = $grp ? "$bld: " : '';
	$where = "$grp/$where" if $ambiguous{$bld};
	my $warn = $na ? 'N/A! ' : '';
	my $changed = $change ? '~' : ' ';
	$add = "!$add" if $warn;
	my $mess = "$changed$add$flag\n  $warn$where$flags{$flag}{$grp}{$bld}";
	my $sum = 0;
	$sum += length for ': ', split "\n  ", $mess;
	$mess = join ': ', split("\n  ", $mess) if $sum < 80;
	s/(?<=^.)([-+!])([^\s:]+)/$color{$1}$1$2\e[0m/ for $mess;
	print $mess, $/;
}
__END__
$s=q#|perl -we XXXprint for grep !$_{$_}++, map $_->[0], sort { $a->[1] cmp $b->[1] or $a->[0] cmp $b->[0] } map { /^[-+](\S+)\s/;$k=$1; $k=~s/^.*\)//; [$_,$k] } <>XXX#; $s=~s/XXX/\x27/g; open STDOUT, $s; my @P = @ARGV; @ARGV = glob "/usr/portage/profiles/use.{,local.}desc"; while (<>) { chomp; $local = s|^(\w+-\w+/[^:]+):|| ? $1 : undef; @F=split; next unless /^\S+ - /; if ($local) { $local{$F[0]}{$local} = $_; } else { $g{$F[0]}=$_; } } @ARGV=("emerge -pv @P|"); while (<>) { chomp; @F=split; next unless /^\[ebuild/; shift @F while $F[0] !~ /\]/; shift @F; $F[0]=~m{^([^/]+/\w+(?:-[A-Za-z]\w+)*)-} and $p = $1; @F=grep /^[-+]/, @F; for (@F) { s/^(.)// and $pm = $1; print $pm, $local{$_}{$p} ? "($p)$local{$_}{$p}" : $g{$_}; } } close STDOUT;' -- --newuse -Du xscreensaver | less
