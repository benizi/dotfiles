package Tag::Apostrophe;

use 5.006;
use strict;
use warnings;

use WordNet;

require Tag::Stoplist;

my @defaults = ( inf_loc  => '/wordnet/wn/2k3/lib/perl/various',
                 afile => 'apos.all');

my $apos_file;
my $apos_initialized = 0;
my $wn;

sub import {
    my @EXPORT = qw(find_and_hideApostrophes restore_Apostrophes);
    my $package = shift;
    my ($caller, $file, $line) = caller();

    use File::Spec;

    my %param = (@defaults, @_);
    $apos_file = File::Spec->catfile($param{inf_loc}, $param{afile});

    Tag::Stoplist->import(@_);
    
    $wn = WordNet->new(@_);
    
    no strict 'refs';
    foreach my $export (@EXPORT) {
        *{"$caller\::$export"} = *{"$package\::$export"};
    }
    _loadApos() if !$apos_initialized;
}

my %apos = ();

sub find_and_hideApostrophes {
    my ($str, $check_known_apos_only) = @_;
    $check_known_apos_only ||= 0;
    
    return $str if $str !~ /'/o;

    # Hide apostrophes in contractions and other single-word forms
    # containing apostrophes. Always test longest to shortest. Case is
    # ignored.
    foreach my $c (sort {length($b) <=> length($a)} keys %Tag::Functions::contractions) {
        $str =~ s/\b$c\b/Tag::Functions::Hide_apos($&)/gie;
    }
    return $str if $str !~ /'/o;
    foreach my $num (sort {$b cmp $a} keys %apos) {
        foreach my $apos (sort {length($b) <=> length($a)} keys %{$apos{$num}}) {
            $str =~ s/(?<=`)\b$apos(?=\')/Tag::Functions::Hide_apos($&)/gie;
            $str =~ s/(?<!(?:`))\b$apos(?=$|\W)/Tag::Functions::Hide_apos($&)/gie;
        }
    }
    return $str if $check_known_apos_only || $str !~ /'/o;

    # Hide apos' that look like they perform a grammatical function.
    # Stop when the # of left quotes equals the # of right quotes (not
    # merely when even).
    while (1) {
        last if (scalar(my @a = ($str =~ /`/go))) == (scalar(my @b = ($str =~ /\'/go)));
        next if $str =~ s/\'s\b/Tag::Functions::Hide_apos($&)/geo;
        next if $str =~ s/(?<!`)\b[A-Z][\w\.]+?\'\W/Tag::Functions::Hide_apos($&)/eo;
        if ( $str =~ /(?<!`)\b([^`\'\s]+?s)\'\W/o ) {
            my $word = $1;
            if ( !is_stoplist($word) && ($wn->is_word($word, "n") || $wn->is_word(substr($word, 0, -1), "n")) ) {
                $str =~ s/(?<!`)\b$word\'\W/Tag::Functions::Hide_apos($&)/e;
                next;
            }
        }
        last;
    }    
    return $str;
}

sub restore_Apostrophes {
    return Tag::Functions::Unhide_apos($_[0]);
}

sub defaults {
    my %defaults = ();
    while (<DATA>) {
        s/[\n\r\s]+?$//go;
        next if !/\w/o;
        $defaults{scalar(my @parts = split ' ', $_)}{$_}++;
    }
    return (\%defaults, $apos_file);
}

sub _loadApos () {
    %apos = ();
    if (!-e $apos_file) {
        warn "$apos_file does not exist\n";
        return;
    }
    open (APOS, "<$apos_file") or die "Couldn't open $apos_file!\n";
    while (<APOS>) {
        s/[\n\r\s]+?$//go;
        next if /^#/o or !/\w/o;
        $apos{scalar(my @parts = split ' ', $_)}{lc $_}++;
    }
    close (APOS);
    $apos_initialized = 1;
}

1;

=head1 NAME

Tag::Apostrophe

=over 4

Functions for apostrophe-handling

=head1 Functions

=item B<find_and_hideApostrophes(>I<$str>, I<$check_known_apos_only>B<)>

=cut

__DATA__
o'clocks
d'oeuvres
howe'er
whoe'er
'twixt
'tween
fann'd
m'aider
chok'd
'cause
'hood
'til
tho'
nor'
sou'
'n'
'un
'em
ol'
d'
o'