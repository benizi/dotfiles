#!/usr/bin/perl
use strict;
use warnings;
use Getopt::Long;
my (@dates, @files);
my $in_bday;
my $rc_file = "$ENV{HOME}/.config/halfplus7";
$in_bday = do $rc_file if -f $rc_file;
GetOptions(
	'current=s' => \ (my $in_now = $ENV{CURRENT}||'now'),
	'my|mine=s' => \$in_bday,
	'debug+' => \ (my $debug = 0),
	'<>' => sub {
		local $_ = shift;
		-f() ? push @files, $_ : push @dates, $_;
	},
) or die 'options';
$in_bday or die 'Must specify --my birthday';
@ARGV = (@files, @ARGV);
use Date::Manip;
sub ordie {
	my $fn = shift;
	my $func;
	{ no strict 'refs'; $func = \&{$fn}; }
	my $new = sub {
		my $r = $func->(@_);
		$debug and warn "$fn(@_) -> $r\n";
		$r or die "Couldn't $fn(@_)\n";
	};
	my $neww = sub {
		my $r = $func->(@_);
		$r or warn "Bad: $fn(@_)\n";
		$debug and warn "$fn(@_) -> $r\n";
		$r;
	};
	{ no strict 'refs'; no warnings 'redefine'; *$fn = $new; *{$fn.'w'} = $neww; }
}
ordie($_) for qw/ParseDate DateCalc Delta_Format/;

my $now = ParseDate($in_now);

my ($bday, $age, $sec, $hp7, $tm7);
sub set_bday {
	$bday = ParseDate($in_bday);
	$age = DateCalc($bday, $now);
	$sec = Delta_Format($age, 0, '%sh');
	$hp7 = DateCalc($now, "-".int($sec/2)." sec");
	$hp7 = DateCalc($hp7, "-7 years");
	$tm7 = DateCalc($now, "-".($sec*2)." sec");
	$tm7 = DateCalc($tm7, "+14 years");
	warn "Date should be born after  ", UnixDate($tm7, '%C'), $/;
	warn "Date should be born before ", UnixDate($hp7, '%C'), $/;
	warn "You're $sec seconds old\n";
}

set_bday($in_bday);

sub comp {
	my $in = shift;
	my $dbd = ref($in) ? $in : ParseDatew($in);
    my $pd = UnixDate($dbd, "%Y/%m/%d");
    my $reason = '';
	my ($l, $g);
    if (Date_Cmp($dbd, $hp7) <= 0 and Date_Cmp($dbd, $tm7) >= 0) {
        print "$pd Hoo rah.\n";
    } elsif (Date_Cmp($dbd, $hp7) > 0) { # too young
        $reason = 'Too young';
        ($l, $g) = ($bday, $dbd);
    } else { # (Date_Cmp($dbd, $hm7) < 0) # too old
        $reason = 'Too old';
        ($l, $g) = ($dbd, $bday);
    }
    if ($reason) {
        my $aab = Delta_Format(DateCalc($l, $g), 0, '%sh');
        my $when = DateCalc($l, '+ 14 years'); # '+ '.(3 * $aab).' sec');
        $when = DateCalc($when, '+ '.(2 * $aab).' sec');
		# use Data::Dumper; die Dumper [$bday,$dbd,$when,$now];
        my ($ma, $da, $mc, $dc) = map int(Delta_Format(DateCalc(@$_), 'approx', 0, '%yd')), [$bday,$when], [$dbd,$when], [$bday,$now], [$dbd,$now];
		my $delta = DateCalc($now, $when, 1);
        print UnixDate($when, $pd." $reason. Wait until: %Y/%m/%d".Delta_Format($delta,'approx',0,' (%yv years, %Mv months)')."\n(You: $ma [currently $mc]) (Date: $da [currently $dc])"), $/;
    }
}

while (<>) {
    chomp;
	if (s/^b(?:irth)?(?:day)?\s*//) {
		set_bday($_);
	} elsif (s/^age\s*//i) {
		$_ .= ' years' unless /[a-z]/i;
		comp(DateCalcw($now, "- $_"));
	} else {
		comp($_);
	}
}
