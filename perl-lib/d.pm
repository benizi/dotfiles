package d;
use datestring;
open my $f, '<', $INC{'datestring.pm'} or die "<datestring: $!";
undef local $/;
my $ds = <$f>;
$ds =~ s/datestring/d/;
eval $ds;
1;
