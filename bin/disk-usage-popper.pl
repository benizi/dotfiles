#!/usr/bin/perl
use strict;
use warnings;
use POSIX 'strftime';
use Tk;
use Statfs;
use Getopt::Long;
GetOptions(
	'delay=i' => \(my $delay_start = 0),
	'wm!' => \(my $no_wm),
	'justbat' => \(my $just_battery = 0),
) or die 'options';
$no_wm //= !length `pgrep ion3`;
sleep $delay_start;
my $mw = MainWindow->new(-title=>'diskusage');
{
	my $cleanedup = 0;	
	sub cleanup {
		next if $cleanedup++;
		Tk::exit(0);
	}
	sub restart { exec { $^X } $^X, $0 }
	sub restart_delay { exec { $^X } $^X, $0, '--delay', 5 }
}
my $geometry = '-0+0';
sub update_geometry { $mw->geometry($geometry) }
sub toggle_geometry {
	$geometry = join '', grep $_, reverse split //, $geometry;
	substr($geometry, 0, 1) =~ tr{-+}{+-};
	$geometry =~ s/(.)/${1}0/g;
}
$mw->bind('<Key-q>', \&cleanup);
$mw->bind('<Button-1>', \&restart);
$mw->bind('<Button-2>', sub { toggle_geometry; update_geometry });
$mw->bind('<Button-3>', \&cleanup);
$mw->overrideredirect(1) if $no_wm;
update_geometry;
my $label_text = '';
my $label = $mw->Label(
	-font=>[qw/Courier 9/]=>
	-justify=>right=>
	-textvariable=>\$label_text
)->pack;
my @temps;
my $temp_path;
my $processors;
my $batteries;
my @warnings;
{
	my @to_check;
	my @prefs = (map "$ENV{HOME}/$_", qw/.duprc .dupmessage/);
	sub pref_info {
		my $info = {};
		$$info{$_} = (!-f) ? '0:0' : join ':', 1, (stat)[9] for @prefs;
		$info;
	}
	my $last = {};
	$$last{$_} = '0:0' for @prefs;
	sub update_prefs {
		my $current = pref_info;
		my @updates = grep $$current{$_} ne $$last{$_}, keys %$last;
		$last = $current;
		if (@updates) {
			local @ARGV = grep $$current{$_} ne '0:0', keys %$current;
			@to_check = ();
			@warnings = ();
			if (@ARGV) { while (<>) {
				chomp;
				my $eq = qr/\s*=\s*/;
				if (s/^temps$eq//) {
					my ($low, $mid, $path) = split;
					$path //= "/proc/acpi/thermal_zone/*/temperature";
					$temp_path = $path;
					@temps = ($low, $mid);
				} elsif (s/^procs$eq//) {
					$processors = $_;
				} elsif (s/^batts$eq//) {
					$batteries = $_;
				} elsif (s/^message(?:$eq)?//) {
					push @warnings, $_;
				} elsif (s/^font$eq//) {
					$label->configure(-font => $_);
				} else {
					push @to_check, $_;
				}
			} }
		}
		@to_check = qw{/ /usr:U /home:H} unless @to_check;
		@to_check;
	}
}
sub update {
	update_prefs;
	my @parts;
	my @warn = @warnings;
	local $_ = '';
	for (grep !$just_battery, update_prefs) {
		my $split = qr/[:\t]/;
		my ($dir, $disp, $limit) = /$split/ ? (split /$split/) : ($_) x 2;
		$limit ||= 0;
		my (@devinos) = map { (stat)[0,1] } $dir, "$dir/..";
		next if $devinos[0] == $devinos[2] and $devinos[1] != $devinos[3];
		$_ .= "" for $dir,$disp;
		my $s = statfsM $dir;
		my $avail = $s->avail;
		my $tot = $s->tot;
		push @parts, sprintf "%2s\0%d\0%d%s",
			$disp, 100 * (1 - ($avail/$tot)), $avail,
			(0?sprintf("\n\0%02d", $tot):'');
		next if $limit and $avail > $limit;
		push @warn, "LOW DISK SPACE!!\n'\U$dir\E' is low!\n($avail M left)"
			if
				($avail < 100 and $avail/$tot < .05) or
				($avail/$tot < .05);
	}
	for (@parts) {
		my @lines = split /\n/;
		for (@lines) {
			my @p = split /\0/;
			splice @p, 0, @p-1, "@p[0..$#p-1]";
			my $len = 0;
			$len += length for @p;
			$_ = $p[0].(" " x (11-$len)).$p[1];
			s/^\s//;
		}
		$_ = join "\n", @lines;
	}
	if ($batteries) {
		local @ARGV = (glob $batteries);
		my $cap = 1;
		my $curr = 0;
		while (<>) {
			/remain.*?(\d+)/ ? ($curr = $1) :
			/last.full.*?(\d+)/ ? ($cap = $1) :
			next;
		}
		my $pct = $curr/$cap;
		push @parts, sprintf "Bat: %03d", $pct*100;
		push @warn, "BATTERY $pct" if $pct < .2;
	}
	if ($temp_path) {
		local @ARGV = (glob $temp_path);
		my ($low, $mid) = @temps;
		while (<>) {
			chomp;
			next unless s/^temperature:\s*//;
			my $c = /(\d+)/ ? $1 : 0;
			my $f = ($c * 1.8) + 32;
			push @parts, sprintf "%02dC / %03dF", $c, $f;
			if ($c >= $mid) {
				push @warn, "HOT HOT HOT";
			} elsif ($c >= $low) {
				push @parts, "heating up";
			}
		}
	}
	if ($processors) {
		my @cpus = (glob $processors);
		my $count = 0 + @cpus;
		# push @parts, "$count CPUS";
		# local @ARGV = (grep -f, glob "$cpu/sysdev/cpufreq/cpuinfo_cur_freq");
		# @ARGV or last;
		# chomp(my @freq = <>);
		# push @parts, "FREQ(@freq)";
		# local @ARGV = (glob "$cpu/thermal_cooling/cur_state");
		# chomp(my @throt = <>);
		# $_ /= 1e6 for @freq;
		# push @parts, sprintf "CPU%01d %3.2fT%d", $_, $freq[$_], $throt[$_] for 0..$#freq;
	}
	{
		push @warn, map "\U$_\E IS UP", grep { -f "/tmp/$_-is-up" } qw/elation wordnet/;
	}
	
	$label_text = join "\n",
		join("\n", strftime("%Y/%m/%d%n%H:%M:%S",localtime), @parts),
		@warn;
	$label->configure('-background',@warn?"#ff9999":"#ffffdd");
	$mw->after(@warn?1000:5000,\&update);
}
update();
MainLoop;
