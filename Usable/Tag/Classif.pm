package Tag::Classif;

# No longer globbing & auto-tagging <classif> info, just recognizing them.

use 5.006;
use strict;
use warnings;

my @defaults = ( inf_loc  => '/wordnet/wn/2k3/lib/perl/various',
                 cfile => 'classif.tok' );

my $cfile;
my $cl_initialized;

sub import {
    my @EXPORT = qw(classif_file classif_regex classif_tag_info);
    my $package = shift;
    my ($caller, $file, $line) = caller();

    use File::Spec;

    my %param = (@defaults, @_);
    $cfile = File::Spec->catfile($param{inf_loc}, $param{cfile});

    no strict 'refs';
    foreach my $export (@EXPORT) {
        *{"$caller\::$export"} = *{"$package\::$export"};
    }
    _loadClassifs() if !$cl_initialized;
}

my %classifs = ();
my $class_re;
my %ptr_to_type = qw(;c cat ;u use ;r reg);

sub classif_tag_info {
    my $classif = shift;
    my @classifs = split /(?:\ +and|,)\ +?(?=[^ ])/, $classif;
    my @tag_info = ();
    foreach my $c (@classifs) {
        $c = defined $classifs{lc $c} ? lc $c : ucfirst $c;
        if ( !defined $classifs{$c} ) {
            return ();
        } else {
            if (!@tag_info) {
                push @tag_info, $c, $classifs{$c}{sk}, $classifs{$c}{class};
            } else {
                $tag_info[0] .= ",$c";
                $tag_info[1] .= ",$classifs{$c}{sk}";
                $tag_info[2] .= ",$classifs{$c}{class}" if $tag_info[2] !~ /^$classifs{$c}{class}$/;
            }
        }
    }
    return @tag_info;
}

sub classif_regex {
    return $class_re;
}

sub classif_file {
    return $cfile;
}

sub _loadClassifs {
    %classifs = ();
    if (!-e $cfile) {
        warn "$cfile does not exist\n";
        return;
    }
    open (FIL, "<$cfile") or die "Couldn't open $cfile!\n";
    while (<FIL>) {
        next if m/^#/;
        s/[\n\r\s]+?$//g;
        my ($lemma, $ptr, $sk) = split /,/;
        $classifs{$lemma}{sk} = $sk;
        $classifs{$lemma}{class} = $ptr_to_type{$ptr};
        $class_re = defined $class_re ? "$class_re|$lemma" : $lemma;
    }
    $class_re =~ s/\./\\./;
    $class_re = qr/$class_re/i;
    close (FIL);
    $cl_initialized = 1;
}

1;

=head1 NAME

Tag::Classif

=over 4

Functions for generating and accessing files for auto-tagging <classif>
(domain/topic) info.

=head1 Functions

=item B<classif_tag_info(>I<classif>B<)>

Takes passed in domain classification text (eg., Biology) and returns
a list of (lemma, sense_key, and class_type) info needed to auto-tag the
text. If the text is composed of more than one domain (eg., "biology,
physics"), then lemma and sense_key will contain a comma-delimited list
of lemmas and sks. class_type will contain a unique list of types.

If the classifier passed in does not have a corresponding synset
in WordNet, or the passed-in text contains more than one classifier
and not all have a corresponding synset, then an empty list will be
returned.

=item B<classif_regex>

Returns a regex of all domain classifiers OR'd together.

=item B<classif_file>

Returns the path+filename for the domain class mapping to wn sense key.

=cut

