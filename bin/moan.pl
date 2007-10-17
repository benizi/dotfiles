#!/usr/csl/bin/perl
use lib '/wordnet/wn/2k3/lib/perl';
use lib "$ENV{HOME}/Usable";
use lib "$ENV{HOME}/WNMorph";
use warnings;
use strict;

use Getopt::Long;
Getopt::Long::Configure(qw/bundling/);
my @wn_opt;
my $debug = 1;
GetOptions( 'file|f=s' => \ (my $file = ''),
            'd+' => \$debug,
            'debug=i' => \$debug,
            'groan|g' => \ (my $groan = 0),
            '<>' => sub { push @wn_opt, @_; },
            'cache|c' => \ (my $cache = 0),
            'irrdir|irr=s' => \ (my $irrdir = 'morphdir'),
            'morphdir|m=s' => \ (my $morphdir = "/wordnet/wn/2k3/lib/perl/various" ),
            ) or die 'usage';

my @opt;
push @opt, ( morphdir => $morphdir,
             irrdir => $irrdir );
push @opt, ( reader => 'Cache' ) if $cache;
push @opt, ( debug  => $debug ) if $debug;
push @opt, ( inf_file => $file ) if $file;

warn "options: Debug-level: $debug -- Cache?: ", ($cache ? "Yes" : "No"), "\n", $file ? "File: $morphdir/$file\n" : "";

use WordNet qw/:pos/;
use WordNet::Morphology;
warn "Initializing...";
my $morph = WordNet::Morphology->new(@opt, @wn_opt);
warn "READY\n\n";

my $func = ($groan ? 'groan' : 'moan');
while (<STDIN>) {
    chomp;
    my @a = split /\t/;
    my ($word, @p) = @a;
    $word ||= "";
    @p = @pos unless @p;
    warn "${func}ing: :: $word :: with parts-of-speech: ". join(",", @p). "\n";
    my @r = $morph->$func($word, @p);
    warn "Results:\n";
    while (@r) {
        my ($w, $p) = splice @r, 0, 2;
        print "$w : $p\n";
    }
    warn "\n";
}
