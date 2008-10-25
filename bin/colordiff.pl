#!/usr/bin/perl
use strict;
use warnings;
use CGI;
use Algorithm::Diff;
use Getopt::Long;
Getopt::Long::Configure(qw/bundling/);
my ($f, $g);
GetOptions(
	'c' => \ (my $show_only_changes = 0),
	'n' => \ (my $no_color = 0),
	'split=s' => \ (my $split),
	's' => \ (my $split_on_spaces = 0),
	'l' => \ (my $split_on_letters = 0),
	'p' => \ (my $split_on_punc = 0),
	'w' => \ (my $split_on_words = 0),
	'h' => \ (my $html_output = 0),
	'b' => \ (my $background = 0),
	'd' => \ (my $debug = 0),
	'<>' => sub {
		unless (defined($f)) { ($f) = @_; return; }
		unless (defined($g)) { ($g) = @_; return; }
		die "too many files";
	},
);

for (
	[ $split_on_letters, qr// ],
	[ $split_on_spaces, qr/(\s+)/ ],
	[ $split_on_words, qr/\b/ ],
	[ $split_on_punc, qr/\b|([-;:\"\'\.+*&^%\$\#@!~+=\\\{\}\[\]\(\)])/ ],
	) {
	my ($t, $qr) = @$_;
	last if $split;
	$split = $qr if $t;
}
$split = qr/$split/ if $split and not ref $split;
$split = qr// unless $split;

my %c = qw/u 0 + 42;37;1 - 41;37;1/;
my %functions;
{
	no strict 'refs';
	for my $func (qw/header removed added unchanged footer preline postline/) {
		*$func = sub { my $f = $functions{$func}; $f ? $f->(@_) : ''; };
	}
}
if ($html_output) {
	%functions = (
		header => sub {
			"<html>\n".
			"<head><title>Diff of $f and $g</title>\n".
			"<style type=\"text/css\">\n".
			"span.removed { color: red; }\n".
			"span.removed { text-decoration: line-through; }\n".
			"span.added { color: blue; }\n".
			"div.removed, div.added, div.changed { padding: 2px; }\n".
			($background
			? "div.removed { background: #fbb; }\n".
				"div.added   { background: #bbf; }\n".
				"div.changed { background: #bfb; }\n"
			: "div.removed { border: 1px red dashed; }\n".
				"div.added { border: 1px blue dashed; }\n".
				"div.changed { border: 1px green dashed; }\n"
			).
			"</style>\n</head>\n<body>\n<div>"
		},
		removed => sub {
			my $e = $_[1] ? 'div' : 'span';
			qq|<$e class="removed">|
			.CGI::escapeHTML($_[0])
			.qq|</$e>|
			.($_[1] ? "\n" : '');
		},
		added => sub {
			my $e = $_[1] ? 'div' : 'span';
			qq|<$e class="added">|
			.CGI::escapeHTML($_[0])
			.qq|</$e>|
			.($_[1] ? "\n" : '');
		},
		unchanged => sub {
			my $t = CGI::escapeHTML($_[0]);
			$_[1] ? "<div>$t</div>\n" : $t;
		},
		footer  => sub { "</div>\n</body>\n</html>\n"         },
		preline => sub { qq|<div class="changed">|            },
		postline => sub { "</div>\n"                          },
	);
} else {
	%functions = $no_color
	? (
		removed => sub { "<*-:$_[0]*>" },
		added   => sub { "<++:$_[0]+>" },
		unchanged => sub { $_[0] },
		postline => sub { "\n" },
	)
	: (
		removed => sub {           "\e[".$c{"-"}."m$_[0]\e[0m"         },
		added   => sub {           "\e[".$c{"+"}."m$_[0]\e[0m"         },
		unchanged => sub { $c{u} ? "\e[".$c{"u"}."m$_[0]\e[0m" : $_[0] },
		postline => sub { "\n"                                         },
	);
	for my $func (qw/removed added unchanged/) {
		my $s = $functions{$func};
		$functions{$func} = sub { $s->(@_) . ($_[1] ? "\n" : '') };
	}
}

for my $func (qw/removed added unchanged/) {
	my $s = $functions{$func};
	$functions{$func} = sub {
		return unless defined $_[0] and length $_[0];
		$s->(@_);
	};
}

s/([\\\'\"\ ])/\\$1/g for (my($fs,$gs)=($f,$g));
my @dc = (
	"diff",
	"-w",
	"--unchanged-group-format=s\n%dn\n%=",
	"--old-group-format=c\n%dn\n%<0\n",
	"--new-group-format=c\n0\n%dN\n%>",
	"--changed-group-format=c\n%dn\n%<%dN\n%>",
	"--old-line-format=%L",
	"--new-line-format=%L",
	"--unchanged-line-format=%L",
	$fs,
	$gs,
);
my @lines;
die "Couldn't pipe: $!" unless pipe my($pread), my($pwrite);
die "Error forking: $!" unless defined(my $pid = fork);
if (!$pid) {
	close $pread;
	select((select($pwrite),$|=1)[0]);
	open STDOUT, '>&', $pwrite or die "Couldn't dup: $!";
	system { $dc[0] } @dc;
	close $pwrite;
	exit;
} else {
	close $pwrite;
	undef local $/;
	chomp(@lines = split /\n/, <$pread>);
	close $pread;
}

print header();
while (@lines) {
    my $op = shift @lines;
    my (@a, @b);
    if ($op eq 's') {
        my @s = splice @lines, 0, shift(@lines);
        unless ($show_only_changes) {
            print unchanged($_, 1) for @s;
        }
        next;
    } else {
        @a = splice @lines, 0, shift(@lines);
        @b = splice @lines, 0, shift(@lines);
    }
    if (@a and @b and @a == @b) {
        my @toprint;
        while (@a and @b) {
            my ($A, $B) = map { [ grep { defined($_) and length } split $split, $_ ] } (shift(@a), shift(@b));
            my $diff = Algorithm::Diff->new($A, $B);
            $debug and warn "\$A:".join('+',@$A)."\n\$B:".join('+',@$B)."\n";
            print preline();
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
            while (my ($type, $what) = splice @toprint, 0, 2) {
                print(($type eq 'u') ? unchanged($what)
                      :
                      ($type eq 'r') ? removed($what)
                      :
                      added($what));
            }
            print postline();
        }
    } else {
        print removed($_, 1) for @a;
        print added($_, 1) for @b;
    }
}
print footer();
