#!/usr/bin/perl
use strict;
use warnings;
use bytes;
use Encode;
use Time::HiRes qw/time/;
use Tk;
die "Couldn't open IPC pipe: $!" unless pipe my $read, my $write;
if (not my $pid = fork) { # CHILD
	die "Couldn't fork: $!" unless defined $pid;
	close $read;
	my $fifo;
	while (1) {
		if (!$fifo or ($fifo and eof $fifo)) {
			unless (open $fifo, '<', '/home/bhaskell/pine-fifo') {
				warn "Couldn't open FIFO: $!";
				syswrite $write, "\x01\n", 2;
				exit;
			}
		}
		my $line = '';
		while ($fifo) {
			my $r = sysread $fifo, $line, 1, length $line;
			die "Error reading FIFO: $!" unless defined $r;
			undef $fifo unless $r;
			syswrite $write, $1, length $1 if $line =~ s/\A([^\n]+\n)//;
		}
	}
	close $write;
	exit;
}
close $write;

my $mw = MainWindow->new(-title=>'traybiff');
{ my $done = 0; sub cleanup { next if $done++; close $read; Tk::exit(0); } }
my $quit = $mw->Button(-text=>'quit',-command=>\&cleanup)->pack;
$mw->bind('<Key-q>', \&cleanup);
my @plist;
sub new_pop {
	my $popup = $mw->Toplevel;
	my $frame2 = $popup->Frame(qw/-borderwidth 1 -background black/)->pack;
	my $frame1 = $frame2->Frame(qw/-borderwidth 3 -background/,'#ff6666')->pack;
	my $frame = $frame1->Frame(qw/-borderwidth 1 -background black/)->pack;
	my $mail_list = $frame->Label(-background=>'#ffffdd'=>-justify=>left=>-text=>shift)->pack;
	$popup->overrideredirect(1);
	$popup->idletasks;
	$popup->update;
	my $destroyed = 0;
	$popup->bind('<Button-1>', sub { $popup->fadetoblack(\$destroyed) });
	$popup->after(5000=>sub{$popup->fadetoblack(\$destroyed) });
	$popup;
}
sub add_pop { my $p = new_pop(@_?shift:"foo\n".time); unshift @plist, $p; }
sub check_mail {
	my $rs = '';
	vec($rs, fileno($read), 1) = 1;
	if (select $rs, undef, undef, .01) {
		my $got = '';
		while (1) {
			my $r = sysread $read, $got, 1, length $got;
			die "Error reading IPC pipe: $!" unless defined $r;
			last if !$r;
			last if $got =~ s/\n//;
		}
		if ($got eq "\x01") {
			warn "Error reading from FIFO. Exiting.\n";
			cleanup;
		}
		$got =~ s/^([^\t]*)\t([^\t]*)\t.*$/$1\n$2/;
		$got =~ s/\A([^\n]+)\n\[\s?([^\]]+?)\s?\]\s*(.*)\Z/[$2]\n$1\n$3/;
		$got = decode_utf8($got) if $got =~ /^
		(?:
		[\x00-\x7f]
		|[\xc0-\xdf][\x80-\xbf]
		|[\xe0-\xef][\x80-\xbf][\x80-\xbf]
		|[\xf0-\xf7][\x80-\xbf][\x80-\xbf][\x80-\xbf]
		)*$/x;
		add_pop($got);
		update();
	}
}
sub update {
	@plist = grep $_->Exists, @plist;
	return unless @plist;
	$plist[-1]->geometry("-0-48");
	$plist[$_]->put_above($plist[$_+1]) for reverse 0..$#plist-1;
}
sub Tk::Toplevel::put_above {
	my ($this, $that) = @_;
	my (@geo) = map $_->geometry, @_;
	my @thi = split /\D/, shift @geo;
	my @tha = split /\D/, shift @geo;
	$thi[-1] = $tha[-1]+$thi[1];
	$this->geometry("-$thi[-2]-$thi[-1]");
}
sub Tk::Toplevel::shrinkit {
	my $this = shift;
	local $_ = $this->geometry;
	s/(?<=x)(\d+)/$1-1/e;
	if ($1 == '1') { $this->destroy; return; }
	$this->geometry($_);
}
sub Tk::Toplevel::fadetoblack {
	my $this = shift;
	my $dest = shift;
	return if $$dest++;
	$this->repeat(20=>sub { $this->shrinkit; update(); });
}
$mw->withdraw;
$mw->repeat(100 => \&check_mail);
$mw->repeat(100 => \&update);
MainLoop;
