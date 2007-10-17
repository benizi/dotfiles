#!/usr/bin/perl
use strict;
use warnings;
use POSIX 'strftime';
use Tk;
my $mw = MainWindow->new(-title=>'diskusage');
{
	my $cleanedup = 0;	
	sub cleanup {
		next if $cleanedup++;
		Tk::exit(0);
	}
}
$mw->bind('<Key-q>', \&cleanup);
$mw->bind('<Button-3>', \&cleanup);
$mw->overrideredirect(1);
$mw->geometry('-1+1');
my $label_text = '';
my $label = $mw->Label(-justify=>right=>-textvariable=>\$label_text)->pack;
sub update {
	local $_ = `df -h /`;
	s/\A[^\n]+\n//;
	s/\A([^\n]+)\n.*\Z/$1/s;
	tr/\t\ /  /s;
	my $warn = (`df -k /` =~ /\s+(\d+)\s100%/ and $1 < 100000) ? "\nLOW DISK SPACE WARNING $1" : "";
	$label_text = strftime("%Y/%m/%d %H:%M:%S",localtime)." $_$warn";
	$label->configure('-background',$warn?"#ff9999":"#ffffdd");
	$mw->after($warn?1000:5000,\&update);
}
update();
MainLoop;
