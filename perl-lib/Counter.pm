package Counter;
use strict;
use warnings;

my $hi_res;
BEGIN { $hi_res = eval 'require Time::HiRes' and Time::HiRes->import('time') }
my $program_start = time;
my $term_supports_title = (($ENV{TERM}||'') =~ /^(?:xterm|linux)$/) ? 1 : 0;
use Getopt::Long;
use base qw/Exporter/;
our @EXPORT = qw/&counter/;
our @EXPORT_OK = @EXPORT;
sub counter {
	shift if @_ and ref $_[0];
	my $options = Getopt::Long::Parser->new;
	$options->configure(pass_through => 'prefix_pattern=|-');
	local @ARGV = @_;
	my $display = '';
	$options->getoptions(
		'display|counter=s' => \$display,
		'title|termtitle|term!' => \ (my $title = $term_supports_title),
		'mod|every=i' => \ (my $mod = 0),
		'wait|time=f' => \ (my $wait = 5),
		'persecond' => \ (my $persec = 0),
		'<>' => sub { $display = shift; },
	);
	my $c = 0;
	my $t = time;
	my $nt = $t;
	my $fmt = $hi_res ? '%5.2f' : '%3d';
	return sub {
		if ($display and (@_ or ($mod ? (!($c % $mod)) : (time > $nt)))) {
			my $time = time;
			my $diff = $time - $t;
			my ($persec_fmt, $persec_info) = ('%s', '');
			if ($persec and $c and $diff) {
				if ($c/$diff > .8) {
					$persec_fmt = '[%5.2f/sec] ';
					$persec_info = $c/$diff;
				} else {
					$persec_fmt = '[%5.2f seconds per] ';
					$persec_info = $diff/$c;
				}

			}
			warn sprintf "%s%s %6d -- %4d $persec_fmt(total: $fmt)\n",
				($title ? "\e]2;$display $c\007" : ""),
				$display, $c, int($diff), $persec_info, ($time-$program_start);
			$nt = $t + $wait * (1 + int($diff/$wait)) unless $mod;
		}
		++$c;
	};
}

1;
