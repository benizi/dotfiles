#!/usr/bin/perl
use strict;
use warnings;

# friendlier error messages for modules not installed
sub nice_use {
	my $module = shift;
	eval "use $module";
	$@ and die "This program requires the $module module\n";
	$module->import(@_);
}
BEGIN { nice_use 'Time::HiRes', 'time' }
BEGIN { nice_use 'IPC::Run', 'run', 'start' }

# these are core modules
use File::Spec;
use Getopt::Long;
GetOptions(
	'quit=s' => \(my $quit_key = 'F2'),
	'window-id|id=s' => \(my $window_id),
	'newline=s' => \(my $split_after = 1.3),
	'backspace!' => \(my $literal_backspace = 1),
	'x11-purist' => \(my $X11_purity = 0),
) or die <<USAGE;
Usage: $0 [options]
  --quit=KEY                 Press KEY to quit [default: F2]
  --window-id=ID / --id=ID   Window ID [default: prompt via xwininfo]
  --newline=DELAY            Delay between starting new lines [default: 1.3]
  --nobackspace              Don't use literal backspaces [default: use]
  --x11-purist               Don't replace weird X11 keysyms [default: do]
USAGE

# Use xwininfo to find a window ID if none specified
if (!defined $window_id) {
	print "Click to choose a window\n";
	run [ 'xwininfo' ], '>', \my $wininfo;
	$wininfo =~ /^(.*Window id.*)$/m
		or die "Couldn't find window ID from: $wininfo\n";
	$window_id = (split ' ', $1)[3];
}

# ignore KeyRelease for modifier keys
my %ignore = map {; $_ => 1 }
	ISO_Level3_Shift =>
	map { ($_."_L", $_."_R") }
	qw/Super Control Alt Shift Meta/;
	
# Use 'xev' to monitor X events
my $events = '';
my $xev = start [ 'xev', '-id', $window_id ], '>', \$events;
my $done = 0;
my @q;

# prints a nicely formatted key for output
my $last_output = time;
sub format_key {
	my $evt = shift;
	my $now = time;

	# Add a newline if there hasn't been a key output in a while
	# Otherwise things just all run together
	if ($split_after and $now - $last_output > $split_after) {
		print "\n";
	}
	$last_output = $now;

	my $key = '';

	if (exists $$evt{string} and $$evt{string} =~ /[[:print:]]/) {
		# Use the looked-up string if it's printable
		$key .= $$evt{string};
	} else {
		# otherwise use the raw key
		$key .= $$evt{key};
	}

	# Convert the modifier state into a nice representation
	# E.g. 0xc becomes Ctrl+Alt+
	my $mods = '';
	$_ = hex for $$evt{state};
	for ( [Ctrl=>4], [Alt=>8], [Shift=>1] ) {
		my ($name, $mask) = @$_;
		$$evt{state} & $mask and $mods .= "$name+";
	}

	# Wrap in 'key' indicators if there are modifiers
	# or it's a named key
	$key = "<$mods$key>" if $mods or 1 < length $key;

	# Shift+W = 'W', for example...
	$key =~ s{Shift\+(?=[^a-z]>)}{};

	# '<:>' is just ':'
	$key =~ s{<(.)>}{$1};

	# If you make a lot of typos (I do), you probably want this
	$key =~ s{<BackSpace>}{\b} if $literal_backspace;

	# Prior? Next? What?
	if (not $X11_purity) {
		for ($key) {
			s/<Prior>/<PageUp>/;
			s/<Next>/<PageDown>/;
			s/<Print>/<PrintScreen>/;
		}
	}

	# Quit on specified key
	$done++ if $$evt{key} =~ /^<?$quit_key>?$/;

	print $key;
}

while (!$done) {
	my $now = time;

	# get input, but don't block on 'xev'
	$xev->pump_nb;

	# if there's input, process it
	if (length $events) {

		# 'xev' events are separated by blank lines
		s/^\n+//, s/\n+$// for $events;
		for (split /\n\n/, $events) {

			# seen = actual time seen
			my $evt = { seen => $now };

			# xtime = time that X server reports
			($$evt{xtime}) = /time (\d+)/;

			# skip unless event = Press or Release
			($$evt{event}) = /^(KeyPress|KeyRelease)/ or next;

			# get the modifier state and keysym
			(@$evt{qw/state key/}) = /^\s*state (0x[\da-f]+).*keysym \S+ (.*?)\)/m;

			# Find the stringified key if present
			/XLookupString gives ([1-9]\d*) bytes/ and
			/XLookupString gives.*?"(.{$1})/sm and
			$$evt{string} = $1;

			push @q, $evt;
		}

		# clear the buffer
		$events = '';
	}

	# test to see whether things that are on the queue should be printed
	# Wait a short time to prevent auto-repeated keys from showing
	my @print;
	push @print, shift @q while @q and $now - $q[0]{seen} > 0.2;
	if (@print) {

		# deal with auto-repeated keys
		my %count;
		$count{$$_{xtime}}++ for @print;

		format_key $_ for grep {
			($$_{event} =~ /Key(Press|Release)/
			#and exists($$_{string})    # printable,
				and !$ignore{$$_{key}},    # non-ignored,
				and $count{$$_{xtime}} < 2 # non-autorepeated,
			) # TODO - expand for mouse events
		} @print;
	}

	# delay so we're not busy-waiting on xev
	select undef, undef, undef, 0.01;
}
