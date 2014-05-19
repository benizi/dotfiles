#!/usr/bin/perl
use strict;
use warnings;
my $VERSION = '0.01';
my $just_fold = $0 =~ /\bfold\b/i;
use Getopt::Long qw/:config pass_through/;
GetOptions(
	'metafy!' => \(my $metafy = 1),
	'fold!' => \(my $fold = 1),
	'min=i' => \(my $min = $just_fold ? 1 : 3),
) or die 'options';
if ($just_fold) { print_fold() for <>; fold_finish(); exit; }
if (eval "use Acme::MetaSyntactic; 1") {
	Acme::MetaSyntactic->import(':all');
} else {
	*metaname = sub { qw/foo bar baz bot qux xyzzy foobar/; };
}
{
	my (%syms, %pre, %pos, %metas);
	sub set_meta {
		my ($type, $meta) = @_;
		$meta eq 'numeric' and $metas{$type} = [ 1..1000 ];
		$metas{$type} ||= [ grep !/_/, metaname($meta||'batman', 1000) ];
	}
	sub set_affix {
		my ($type, $pr, $po) = @_;
		$pre{$type} = $pr;
		$pos{$type} = defined($po) ? $po : '';
	}
	sub gen_new {
		my ($t, $val) = @_;
		my $m = $metas{$t};
		$m or $m = set_meta($t,'');
		my $h = ($syms{$t} ||= {});
		$$h{$val}||=$pre{$t}.$$m[(-1+keys%$h)%@$m].(int(keys(%$h)/@$m)||'').$pos{$t};
	}
	sub fixed { my ($t, $key, $val) = @_; $syms{$t}{$key} = $val; }
}
set_meta(pid => 'numeric');
set_affix(pid => '', ':');
set_affix(addr => '0x{', '}');
set_affix(file => '<', '>');
fixed(file=> 0, 'STDIN');
fixed(file=> 1, 'STDOUT');
fixed(file=> 2, 'STDERR');
while (<>) {
	if ($metafy) {
		s/^(\d+)(?=\s)/gen_new(pid=>$1)/e;
		s/(0x[\da-f]+\b)/gen_new(addr=>$1)/eg;
		s/((?:open|socket)\(.*\)\s=\s)(\d+)/$1.gen_new(file=>$2)/e;
		s/\b((?:fstat64|fstat|close|access|write|read|fcntl64|fcntl|connect)\()(\d+)\b/$1.gen_new(file=>$2)/e;
	}
	s/(?<=gettimeofday\(\{)\d+,\s\d+/timeval/;
	print_fold();
}
fold_finish();

{
	my ($curr, $repeat);
	my $line;
	sub print_fold {
		print and return if !$fold;
		if (defined $curr and $curr eq $_) {
			$repeat++;
		} else {
			fold_finish();
			print;
		}
		$curr = $_;
	}
	sub fold_finish {
		$fold or return;
		if ($repeat) {
			if ($repeat >= $min) {
				print "[ last message repeated $repeat time".($repeat>1?"s":"")." ]\n";
			} else {
				print $curr x $repeat;
			}
		}
		$repeat = 0;
	}
}
