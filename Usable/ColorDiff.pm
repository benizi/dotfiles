package ColorDiff;
use strict;
use warnings;
use Exporter;
our @ISA = qw/Exporter/;
our @EXPORT_OK = qw/&color_diff &color_diff_files/;
use CGI;
use Algorithm::Diff;
use Getopt::Long;

my $show_only_changes = 0;
my $no_color = 0;
my $html_output = 0;
my $background = 0;
my $all_lines = 0;
our $debug = 0;

my $split;
sub Split {
    my $t = shift;
	$split = $t and return if ref $t;
    $t ||= 'l';
    $split = '';
    for (
		[ qr/^l(?:etters?)?$/i, qr// ],
		[ qr/^U(?:nicode?)?$/i, qr/(\X)/ ],
		[ qr/^s(?:paces?)?$/i, qr/(\s+)/ ],
		[ qr/^w(?:ords?)?$/i, qr/\b/ ],
		[
			qr/^p(?:unc(?:t(?:uation)?)?)?$/i,
			qr/\b|([-;:\"\'\.+*&^%\$\#@!~+=\\\{\}\[\]\(\)])/
		],
		[ qr/^c(?:ustom)?$/i, shift || '' ],
		) {
        my ($m, $qr) = @$_;
		next if $t !~ $m;
        $split = $qr;
		last;
    }
    $split = qr/$split/ if $split and not ref $split;
    $split = qr// unless $split;
}
Split('l');
my $format;
our $ignore_same;
sub Format {
    my $f = shift;
	$ignore_same = 0 unless defined $ignore_same;
	if ($f =~ /^(i(?:gnore)?|no|inc?(?:lude)?)(?:same|comm(?:on)?)?$/) {
		$ignore_same = ($1 =~ /in/) ? 0 : 1;
		return;
	}
	if ($f =~ /^(?:all[\s_\-]?(?:lines?)?)$/) {
		$all_lines = 1;
		return;
	}
    $format = '';
    for (
		[ qr/^Tk$/i, 'Tk' ],
		[ qr/^h(?:tml)?$/i, 'HTML' ],
		[ qr/^ANSI$/i, 'ANSI' ],
		[ qr/^(?=.+)(?:plain)?(?:text)?$/i, 'plain' ],
		) {
		my ($m, $v) = @$_;
		$format = $v if $f =~ $m;
		last if $format;
	}
}
Format('ANSI');
my $preprocess;
sub svn_strip {
	local $_ = @_ ? shift : $_;
	s/\$(Date|LastChangedDate|Revision|LastChangedRevision|Rev|Author|LastChangedBy|HeadURL|URL|Id)(?::[^\$]*)?\$/\$$1\$/g;
	$_;
}
sub Preprocess {
	my $p = shift;
    for (
		[ qr/^svn(?:-?key(?:words?)?)?$/i, \&svn_strip ],
		) {
		my ($m, $v) = @$_;
		$preprocess = $v if $p =~ $m;
		last if $preprocess;
	}
}
Preprocess('');

sub import {
    my @options_at = grep { $_[$_] =~ /^(?:format|split|preprocess)$/ } 0..$#_;
    while (@options_at) {
        my ($loc) = pop @options_at;
        my ($opt, $val) = splice @_, $loc, 2;
        die "Option $opt requires an argument in import" unless defined $val;
        $opt = lc $opt;
        if ($opt eq 'format') { Format($_) for split /-/, $val; }
        elsif ($opt eq 'split') { Split($val); }
        elsif ($opt eq 'preprocess') { Preprocess($val); }
    }
    $_[0]->SUPER::export_to_level(1, @_);
}

my %c = qw/u 0 + 38;5;28;7 - 31;7/;
my %functions;
{
    no strict 'refs';
    for my $func (qw/header removed added unchanged footer preline postline/) {
        *$func = sub { my $f = $functions{$format}{$func}; $f ? $f->(@_) : (); };
    }
}
sub _div_span {
	my $class = shift;
	local $_ = shift;
	my ($div) = @_;
	my $el = $div ? 'div' : $class ? 'span' : '';
	$class = $class ? qq{ class="$class"} : '';
	my ($st, $et) = $el ? ("<$el$class>", "</$el>") : ('', '');
	my $end = $div ? "\n" : '';
	s/\t/    /g;
	$_ = CGI::escapeHTML($_);
	$st.$_.$et.$end;
}
$functions{HTML} = {
    header => sub {
		<<'PRECSS'.($background?<<'BG':<<'BORDER').<<'POSTCSS'
<html>
<head><title>Diff</title>
<style type="text/css">
span.removed { color: red; }
span.removed { text-decoration: line-through; }
span.added { color: blue; }
#diff { white-space: pre; font-family: monospace; }
div.removed, div.added, div.changed { padding: 2px; }
PRECSS
div.removed { background: #fbb; }
div.added   { background: #bbf; }
div.changed { background: #bfb; }
BG
div.removed { border: 1px red dashed; }
div.added { border: 1px blue dashed; }
div.changed { border: 1px green dashed; }
BORDER
</style>
</head>
<body>
<div id="diff">
POSTCSS
	},
    removed => sub { _div_span(removed => @_) },
    added => sub { _div_span(added => @_) },
    unchanged => sub { _div_span('' => @_) },
    footer  => sub { "</div>\n</body>\n</html>\n"         },
    preline => sub { qq|<div class="changed">|            },
    postline => sub { "</div>\n"                          },
};
sub ansi_color {
	my ($color, $text) = @_;
	return $text unless $color;
	my @lines = split /\n/, $text, -1;
	join "\n", map length() ? "\e[${color}m$_\e[0m" : $_, @lines;
}
$functions{ANSI} = {
	removed => sub { ansi_color $c{'-'}, $_[0] },
	added   => sub { ansi_color $c{'+'}, $_[0] },
	unchanged => sub { ansi_color $c{u}, $_[0] },
    postline => sub { "\n"                     },
};
$functions{plain} = {
    removed => sub { "<*-:$_[0]*>" },
    added   => sub { "<++:$_[0]+>" },
    unchanged => sub { $_[0] },
    postline => sub { "\n" },
};
$functions{Tk} = {
    removed => sub { [ $_[0], 'removed' ] },
    added => sub { [ $_[0], 'added' ] },
    unchanged => sub { [ $_[0], 'unchanged' ] },
};

sub color_diff_files {
	@_ = map {
		open my $f, '<', $_ or die "opening $_: $!";
		$_ = [ <$f> ];
		$preprocess and @$_ = map $preprocess->($_), @$_;
		chomp @$_;
		$_
	} @_;
	&color_diff;
}
sub color_diff {
	my $refs = grep ref, @_;
	warn "Bad arguments to color_diff\n" and return unless @_==2 or ($refs % 2);
	return &super_color_diff if $refs;
    my @toprint;
    my ($A, $B) = map [
		grep { defined($_) and length }
		((ref($split)||'') eq 'CODE')
		? $split->($_)
		: split $split
	], @_;
    my $diff = Algorithm::Diff->new($A, $B);
    while ($diff->Next) {
        if (my @s = $diff->Same) {
            push @toprint, 'u', join '', @s;
            $debug and warn "@toprint[-2,-1]\n";
        } else {
            push @toprint, 'r', join '', $diff->Items(1);
            $debug and warn "@toprint[-2,-1]\n";
            push @toprint, 'a', join '', $diff->Items(2);
            $debug and warn "@toprint[-2,-1]\n";
        }
    }
    for (reverse 0..$#toprint-2) {
        next if $_ % 2;
        if ($toprint[$_] eq $toprint[$_ + 2]) {
            splice @toprint, $_, 4, $toprint[$_], join '', @toprint[$_+1, $_+3];
        }
    }
    my @ret;
    while (my ($type, $what) = splice @toprint, 0, 2) {
        push @ret, (
			($type eq 'u') ? (grep !$ignore_same, unchanged($what)) :
			($type eq 'r') ? removed($what) :
			added($what)
		);
    }
    @ret;
}

sub super_color_diff {
	warn "Bad args to color_diff\n" and return if 2 != grep 'ARRAY' eq ref, @_;
	chomp @$_ for my ($f, $g) = @_;
	my $diff = Algorithm::Diff->new($f, $g);
	my @ret;
	while ($diff->Next) {
		if (my @same = $diff->Same) {
			push @ret, u => $_.$/ for @same;
		} else {
			my @l = $diff->Items(1);
			my @r = $diff->Items(2);
			if (@l == @r or $all_lines) {
				{
					local $ignore_same = 0;
					for my $i (0..$#l) {
						push @ret, done => $_ for color_diff($l[$i],$r[$i]);
						push @ret, done => $/;
					}
				}
			} else {
				push @ret, r => $_.$/ for @l;
				push @ret, a => $_.$/ for @r;
			}
		}
	}
	for (reverse grep !($_ % 2), 0..$#ret-3) {
		next unless $ret[$_] eq $ret[$_+2];
		splice @ret, $_, 4, $ret[$_], join '', @ret[$_+1,$_+3];
	}
	my @real;
	while (my ($type, $what) = splice @ret, 0, 2) {
		push @real,
			($type eq 'u') ? (grep !$ignore_same, unchanged($what)) :
			($type eq 'r') ? removed($what) :
			($type eq 'a') ? added($what) :
			($type eq 'done') ? $what :
			die "Bad type '$type' in super_color_diff\n";
	}
	header(), @real, footer();
}

1;
