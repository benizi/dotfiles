#!/usr/bin/perl
use strict;
use warnings;
use POSIX 'strftime';
use Tk;
use Statfs;
use Getopt::Long;
GetOptions(
	'delay=i' => \(my $delay_start = 0),
	'wm' => \(my $no_wm = !length(`pgrep ion3`)),
) or die 'options';
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
sub update {
	my @parts;
	my @warn;
	local $_ = '';
	for (qw{/ /usr:U /usr/portage/distfiles:D /home:H /wordnet:W /var/lib/postgresql:P /home/bhaskell/photomosaic/perl/raw:R}) {
		my ($dir, $disp) = /:/ ? (split /:/) : ($_) x 2;
		my (@devinos) = map { (stat)[0,1] } $dir, "$dir/..";
		next if $devinos[0] == $devinos[2] and $devinos[1] != $devinos[3];
		$_ .= "" for $dir,$disp;
		my $s = statfsM $dir;
		my $avail = $s->avail;
		my $tot = $s->tot;
		push @parts, sprintf "%2s\0%d\0%d%s",
			$disp, 100 * (1 - ($avail/$tot)), $avail,
			(0?sprintf("\n\0%02d", $tot):'');
next if $dir eq '/home' and $avail > 200;#FIXME
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
	if(0){#new laptop
		local @ARGV = (glob '/proc/acpi/battery/BAT0/{info,state}');
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
	if(0){#new laptop
		local @ARGV = ('/proc/acpi/thermal_zone/THRM/temperature');
		while (<>) {
			chomp;
			next unless s/^temperature:\s*//;
			my $c = /(\d+)/ ? $1 : 0;
			my $f = ($c * 1.8) + 32;
			push @parts, sprintf "%02dC / %03dF", $c, $f;
			if ($c >= 65) {
				push @warn, "HOT HOT HOT";
			} elsif ($c >= 60) {
				push @parts, "heating up";
			}
		}
	}
	
	$label_text = join "\n",
		join("\n", strftime("%Y/%m/%d%n%H:%M:%S",localtime), @parts),
		@warn;
	$label->configure('-background',@warn?"#ff9999":"#ffffdd");
	$mw->after(@warn?1000:5000,\&update);
}
update();
MainLoop;
