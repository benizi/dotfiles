use 5.006;
use strict;
use warnings;

require Tag::TextFunctions;

my @defaults = ( inf_loc  => '/wordnet/wn/2k3/lib/perl/various',
                 px_file => 'prefix.ix',
                 fw_file => 'fn_words.ix',
                 hm_file => 'hyp_misc.ix',
                 vfile => 'abbrevs.tok' );

package Tag::IndexFunctions;

#TODO: put code in get_all_forms() for upper and lower case versions of small words,
#      prefixes, and the lc'd version of ucletter-lcword, eg., X-ray & x-ray

use Tag::Functions qw/cntChar/;

my ($px_file, $fw_file, $hm_file);
my $ix_initialized;

sub import {
    my @EXPORT = qw(get_all_forms assign_ids_to_wfs next_id index_info_formatted_for_sql);
    my $package = shift;
    my ($caller, $file, $line) = caller();

    use File::Spec;

    my %param = (@defaults, @_);
    $px_file = File::Spec->catfile($param{inf_loc}, $param{px_file});
    $fw_file = File::Spec->catfile($param{inf_loc}, $param{fw_file});
    $hm_file = File::Spec->catfile($param{inf_loc}, $param{hm_file});

    Tag::Abbreviations->import(@defaults, @_);
   
    no strict 'refs';
    foreach my $export (@EXPORT) {
        *{"$caller\::$export"} = *{"$package\::$export"};
    }
    _initIx() if !$ix_initialized;
}

my %fn_word_list_by_value = ();
my ($prefixes, $hyp_misc);

sub get_all_forms {
    my ($lemma, $strip_punc) = @_;
    $strip_punc ||= 0;
    
    # Strip punctuation--the wf_index does not retain punctuation,
    # except for abbreviations.
    $lemma = _normalize($lemma) if $strip_punc;

    my %all_forms;
    $all_forms{$lemma} = 1;
    if ( $lemma =~ /\-/ ) {
        my @forms_thusfar;
        my @words = split ' ', $lemma;
        foreach my $word (@words) {
            if ( $word =~ /\-/ ) {
                # \x09 = close up/keep, \x7F = space replace/keep,
                # "-", \x0D = keep/close up/space replace

                # Do subs for misc items for which want to pre-empt replacing "-"
                # For example: ibn-M..., al-M..., der-S, etc.
                foreach my $misc (sort {length($b) <=> length($a)} keys %$hyp_misc) {
                    $word =~ s/(?=^|\b)($misc)(?=$|\b)/_do_subs($1,"-","\x0D")/e;
                }
                
                # Find and do subs for prefixes that can close up, but
                # not stand alone
                foreach my $pfx (sort {length($b) <=> length($a)} keys %$prefixes) {
                    if ( $word =~ s/^($pfx)/_do_subs($1,"-","\x09")/ie ) {
                        last;
                    }
                }

                # Find and do subs for hyphens that can be replaced w/ spaces, but
                # not closed up...

                # Function words (check list against word from longest to shortest...)
                foreach my $key (sort {$b <=> $a || length($b) <=> length($a)} keys %fn_word_list_by_value) {
                    foreach my $value (@{$fn_word_list_by_value{$key}}) {
                        $word =~ s/(?=^|\b)($value)(?=$|\b)/_do_subs($1,"-","\x7F")/gie;
                    }
                }

                # Don't close up Bose-Einstein, i-Mazur, etc. but do allow 
                # V-J day and X-OR.
                $word =~ s/([a-z]\-[A-Z])/_do_subs($1,"-","\x7F")/ge;
                
                # Don't close up 1st-, 2nd-, 3rd-, etc.
                $word =~ s/((?:1st|2nd|3rd|\dth)\-)/_do_subs($1,"-","\x7F")/ge;
                
                # Don't close up number followed by lower-cased word,
                # or a number (so disallows 12tone, 3membered, 911 for
                # 9-11, but allows 401k and 3D.)
                $word =~ s/(\d\-)(?=([a-z]{2,}|\d))/_do_subs($1,"-","\x7F")/ge;

                # After words ending with 's or ', or before words beginning with '
                $word =~ s/((?:\-')|(?:'s?\-))/_do_subs($1,"-","\x7F")/ge;

                # Now determine all versions based on subs
                
                my @versions_this_word;

                # Plain old hyphens get 3 forms: with hyphen, space instead, closed up
                $word =~ s/\x0D/-/g;
                push @versions_this_word, $word;
                if ( $word =~ /\-/ ) {
                    push @versions_this_word, $word, $word;
                    $versions_this_word[$#versions_this_word] =~ s/\-//g;
                    $versions_this_word[$#versions_this_word-1] =~ s/\-/ /g;
                }

                # Replacements for restricted forms: 2 forms for hyphens following
                # prefixes (w/ hyphen & closed up) and 2 forms for hyphens around
                # function words (w/ hyphens(s) & with hyphens replaced
                # by spaces)

                # replace \x09 with "-" and ""
                @versions_this_word = _do_replacements("\x09", "", @versions_this_word);
                # replace \x7F with "-" and " "
                @versions_this_word = _do_replacements("\x7F", " ", @versions_this_word);
                
                # Now combine with forms collected up to this point
                my @temp;
                foreach my $ver ( @versions_this_word ) {
                    if (!@forms_thusfar) {
                        push @temp, $ver;
                    } else {
                        foreach my $thus ( @forms_thusfar ) {
                            push @temp, "$thus $ver";
                        }
                    }
                }
                @forms_thusfar = @temp;
            } elsif (!@forms_thusfar) {
                push @forms_thusfar, $word;
            } else {
                for (my $i=0; $i<@forms_thusfar; $i++) {
                    $forms_thusfar[$i] .= " $word";
                }
            }
        }
        # Add forms to hash, eliminating any duplicates
        %all_forms = map {$_ => 1} @forms_thusfar;
    }
    
    my @all_forms = keys %all_forms;
    return \@all_forms;
}

sub _do_subs {
    my ($str, $char, $repl) = @_;
    $str =~ s/$char/$repl/g;
    return $str;
}

sub _do_replacements {
    my ($subs, $repl, @versions) = @_;
    my $v_elts = @versions;
    
    for (my $i=0; $i<$v_elts; $i++) {
        my $ver = $versions[$i];
        if ( $versions[$i] =~ s/$subs/-/g ) {
            push @versions, $ver;
            $versions[$#versions] =~ s/$subs/$repl/g;
        }
    }
    return @versions;
}

sub assign_ids_to_wfs {
    my ($str, $next_id) = @_;
    $next_id ||= 1;
    
    my $new_str;
    {
        # Skip tag="ignore" aux's & mwf's
        $str =~ s/^\s*<aux[^>]* tag="ignore"[^>]*>.*?<\/aux>\s*?(?=$|<)// && do {$new_str .= $&; redo;};
        $str =~ s/^\s*<mwf[^>]* tag="ignore"[^>]*>.*?<\/mwf>\s*?(?=$|<)// && do {$new_str .= $&; redo;};
        # Skip start/end tags except for wfs
        $str =~ s/^\s*<\/?(?!wf)[^>]*?>\s*?(?=$|<)// && do {$new_str .= $&; redo;};
        $str =~ s/^<wf.+?<\/wf>\s*?(?=$|<)// && do {$new_str .= _set_wf_id_num($&, $next_id); redo;};
    }
    return $new_str;
}

sub next_id {
    my ($attr, $str) = @_;
    my $id = 0;
    while ( $str =~ s/$attr="(\d+)"// ) {$id = $id < $1 ? $1 : $id;}
    return ++$id;
}

# Assigns a wf-num to the passed-in $wf, after first incrementing the counter.
# Every wf gets a wf-num, regardless of whether it has a lemma! This is so that
# Jin can keep track of things...
#
# This is an internal routine for use by &assign_ids_to_wfs() only, so is not
# exported.
sub _set_wf_id_num {
    my $wf = $_[0];
#    my $lemma = Tag::TextFunctions::getAttributeValue("lemma", $wf);
    return Tag::TextFunctions::setAttributeValues($wf, {'wf-num' => $_[1]++});
}

# Normalize index form of a lemma--strip periods if is not an
# abbreviation, strip all other punctuation.
sub _normalize {
    my $word = shift;
    $word =~ s/[:;,!\(\)\?]//g;
    $word =~ s/\.(?!\d)//g if !is_abbreviation($word);
    return $word;
}

sub index_info_formatted_for_sql {
    my ($str, $synset_id, $which_field, $wfi_ref, $stag_ref) = @_;
    $wfi_ref ||= []; # if !defined($wfi_ref);
    $stag_ref ||= []; # if !defined($stag_ref);
 
    if ( !defined($which_field) || $which_field !~ /^(?:gloss|examples)/ ) {
        $str =~ /<gloss[^>]*?>(.+?)<\/def>/;
        return (undef, undef) if !defined($1);
        ($wfi_ref, $stag_ref) = index_info_formatted_for_sql($1, $synset_id, "gloss", $wfi_ref, $stag_ref);
        if ( $str =~ /(<\/def>(?:<aux.+?<\/aux>)*)(<ex.+?)(?=<\/gloss)/ ) {
            ($wfi_ref, $stag_ref) = index_info_formatted_for_sql($2, $synset_id, "examples", $wfi_ref, $stag_ref);
        }
    } else {
        # process all <wf>'s with lemmas and id's
        while ( $str =~ s/<wf[^>]*?>.+?<\/wf>// ) {
            my $wf = $&;
            my $lemma = Tag::TextFunctions::getAttributeValue("lemma", $wf);
            my $wf_num = Tag::TextFunctions::getAttributeValue("wf-num", $wf);
            my $tagtype = Tag::TextFunctions::getAttributeValue("tag", $wf);

            next if !defined($lemma) || $tagtype eq "ignore" || !defined($wf_num);

            my @pairs = split /\|/, $lemma;
            my %already_seen;
            foreach my $wordform_and_pos (@pairs) {
                my ($wordform, $pos) = split /\%/, $wordform_and_pos;
                $wordform = _normalize($wordform);
                unless ( defined($already_seen{$wordform}) ) {
                    $already_seen{$wordform}++;
                    push @$wfi_ref, "$wf_num\t$which_field\t$wordform\t$synset_id\t$tagtype";
                }
            }
            # Don't put multiple-tagged items into the sense_tags index!
            next if $tagtype eq "un";

            # process <id/>'s for this wf
            while ( $wf =~ s/<id[^>]*?>// ) {
                my $id = $&;
                my $sk = Tag::TextFunctions::getAttributeValue("sk", $id);
                my $id_num = Tag::TextFunctions::getAttributeValue("id-num", $id);
                # Every <id/> has exactly one lemma, so no need
                # to a) check if exists, and b) split out multiples
                my $lemma = Tag::TextFunctions::getAttributeValue("lemma", $id);
                $lemma =~ s/%\d+?$//;
                $lemma = _normalize($lemma);
                push @$stag_ref, "$id_num\t$lemma\t$synset_id\t$tagtype\t$sk";
            }
        }        
    }
    return ($wfi_ref, $stag_ref);
}

sub _initIx () {
    $ix_initialized = 1;
    $prefixes = _loadExFile($px_file);
    $hyp_misc = _loadExFile($hm_file);
    _loadFwords();
}

sub _loadExFile {
    my $file = shift;
    my %hash;
    open (FIL, "<$file") or die "Couldn't open $file!\n";
    while (<FIL>) {
        next if m/^#/;
        s/[\n\r\s]+?$//g;
        $hash{$_}++;
    }
    close (FIL);
    return \%hash;
}

sub _loadFwords {
    open (FIL, "<$fw_file") or die "Couldn't open $fw_file!\n";
    my %fn_words;
    while (<FIL>) {
        next if m/^#/;
        s/[\n\r\s]+?$//g;
        $fn_words{$_} = cntChar($_, "-");
    }
    while ( my ($key, $value) = each %fn_words) {
        push @{$fn_word_list_by_value{$value}}, $key;
    }
    close (FIL);
}

package Tag::Abbreviations;

my $abbr_file;
my $abbr_initialized = 0;

sub import {
    my @EXPORT = qw(check_and_tag_Abbreviation is_abbreviation);
    my $package = shift;
    my ($caller, $file, $line) = caller();

    use File::Spec;

    my %param = (@defaults, @_);
    $abbr_file = File::Spec->catfile($param{inf_loc}, $param{vfile});

    no strict 'refs';
    foreach my $export (@EXPORT) {
        *{"$caller\::$export"} = *{"$package\::$export"};
    }
    _loadAbbrevs() if !$abbr_initialized;
}

# %abbrevs hash holds all abbreviations in wn to be sense-tagged automatically. It is
# keyed by the actual form as found in the text. The value of the hash is a hash
# containing sense key(s) and optional rdf. Sense key contains an array
# ref, either to an array of sense key(s), or to an array of hash refs of sense key
# dependencies. Structure is:
#    $abbrevs{$wordform}->{form} (required)
#
# and
#    $abbrevs{$wordform}->{rdf} (optional)
#    $abbrevs{$wordform}->{sk}->[0] (only one sense key)
# or
#    $abbrevs{$wordform}->{sk}->[0] (first sense key)
#    $abbrevs{$wordform}->{sk}->[1] (second sense key)
#                .
#                .
#                .
# or
#    $abbrevs{$wordform}->{sk}->[0]->{sk}     (Assign this sense key...)
#    [$abbrevs{$wordform}->{sk}->[0]->{prevword} (but only if prevword matches this)]
#    $abbrevs{$wordform}->{sk}->[1]->{sk}     (Assign this sense key...)
#    [$abbrevs{$wordform}->{sk}->[1]->{prevword} (but only if prevword matches this)]
#                .
#                .
#                .
# or
#    $abbrevs{$wordform}->{sk}->[0]->{sk}     (Assign this sense key...)
#    [$abbrevs{$wordform}->{sk}->[0]->{prevword} (but only if prevword matches this)]
#    $abbrevs{$wordform}->{sk}->[1]->{sk}     (else assign this sense key...)

my %abbrevs = ();

sub check_and_tag_Abbreviation {
    my ($wf, $prevword, $xml_str) = @_;
    my $contents = ($wf =~ /<wf[^<>]*>(.+?)<\/wf>/) ? $1 : undef;
    $contents =~ s/\'[sd]$//;
    my ($lemma_form, $rdf, $lemma_sk_pairs) = is_abbreviation($contents, $prevword);
    return $wf if !defined $lemma_form;

    my %attrs = ();
    $attrs{rdf} = $rdf if defined $rdf;
    $attrs{type} = "abbr";

    if ( @$lemma_sk_pairs ) {
        # sense tag to all senses matched (lemma assignment is handled there)
        $wf = Tag::TextFunctions::sense_tag_tf($wf, $lemma_sk_pairs, "auto", $xml_str);
    } else {
        # The form is on the abbrev list, but there's no sns_key info 
        # for it, so is an abbrev that is not in wn. These get assigned
        # tag="ignore"
        $attrs{lemma} = $lemma_form;
        $attrs{tag} = "ignore";
    }

    return Tag::TextFunctions::setAttributeValues($wf, \%attrs);
}

sub is_abbreviation {
    my ($word, $prevword) = @_;
    $prevword ||= "";
    return () if !defined $word  || !defined $abbrevs{$word}->{form};
    my $rdf = $abbrevs{$word}->{rdf};
    my $lemma_form = defined $rdf ? $rdf : $word;
    my @lemma_sk_pairs = ();
    if ( $abbrevs{$word}->{sk} ) {
        # Abbrev file format allows for multiple sense keys per
        # abbreviation, as well as some dependency logic based on
        # what the previous word is. Step through each
        # sk+dependency criterion here. . .
        foreach my $sk_inf (@{$abbrevs{$word}->{sk}}) {
            my $sk;
            if ( ref($sk_inf) ) {
                # One of potentially multiple sense keys. . .
                if ( defined($sk_inf->{prevword}) ) {
                    # Is a dependency, check if valid
                    my $regex = qr{$sk_inf->{prevword}};
                    next if $prevword !~ /$regex/;
                }
                $sk = $sk_inf->{sk};
            } else {
                $sk = $sk_inf;
            }
            push @lemma_sk_pairs, $lemma_form, $sk;
        }
        # wf is on the list of abbrevs but fails the tests for 
        # for dependencies, so is NOT an abbreviation
        return () if !@lemma_sk_pairs;
    }
    return $lemma_form, $rdf, \@lemma_sk_pairs;
}

sub _loadAbbrevs () {
    open (ABBREV, "<$abbr_file") or die "Couldn't open $abbr_file!\n";
    $abbr_initialized = 1;
    while (<ABBREV>) {
        next if m/^\#/;
        s/[\n\r\s]+?$//g;
        next unless /[^\s]/;
        my ($abb, $rdf);
        my @sns_keys;

        # Parse line
        s/^([^,]+?)(?:$|,)//;
        $abb = $1;
        if ( s/,?rdf=([^,]+?)(?=$|,)// ) {
            $rdf = $1;
        }
        if ( s/^,?sns_key=(.+?)$// ) {
            # Straightforward sense key
            push @sns_keys, $1;
        } elsif ( /^\{[^\}]+?\}&\{/ ) {
            # Multiple sense keys
            while ( s/^\{sns_key=([^\}]+?)\}(?:$|\&)// ) {
                push @sns_keys, $1;
            }
        } elsif ( /^,?\{sns_key=[^,]+?,prevword=/ ) {
            # Some flavor of dependency (if-then, or if-then-else)
            my $href;
            while ( s/^,?\{sns_key=([^,]+?)(,prevword=[^\}]*?)?\}(?:$|<=>)// ) {
                my $sk = $1;
                my $prevword = $2;
                if ( defined($prevword) ) {
                    $prevword =~ /^,prevword=(.+?)$/;
                    $href = {sk => $sk, prevword => $1}
                } else {
                    $href = {sk => $sk}
                }
                push @sns_keys, $href;
            }
        }
        warn "$abbr_file corrupt at line $." if $_ ne "";

        $abbrevs{$abb}->{form} = $abb;
        $abbrevs{$abb}->{sk} = \@sns_keys if @sns_keys;
        $abbrevs{$abb}->{rdf} = $rdf if defined($rdf);
    }
    close (ABBREV);
}

1;

__END__

=head1 NAME

Tag::IndexFunctions and Tag::Abbreviations

=over 4

Various functions for dealing with indexing and abbreviation-handling. 

=back

=head1 Functions for Tag::IndexFunctions

=over 4

=cut

=item B<get_all_forms(>I<$lemma>B<)>

Takes passed-in lemma form and returns an array of all forms of the
word/collocation in terms of hyphenation, spacing, and
punctuation differences. Certain words will be returned in both upper
and lower case.

e.g., about-face, about face, aboutface; ad-lib, adlib, ad lib; re-examine,
reexamine; ram's-head, ram's head; Shih-Tzu, Shih Tzu; Robert E Lee (minus the period)

No attempt is made to insert spaces/hyphens into juxtaposed forms, only
to determine variants where there are hyphens in the input form.

=cut
 
=item B<assign_ids_to_wfs(>I<$xml_tagged_string>, I<$next_id>B<)>

Takes passed-in text and assigns id numbers to all ID-able wfs, starting
at $next_id. <wf>s that are within ignorable <aux>'s are skipped, since
these wfs will never be auto/manually tagged. All other wfs are passed to
_set_wf_id_num() whether ignorable or not (since some ignorable wfs go into the index).

The passed-in text should be tokenized (wf-tagged) but no ids on wfs should be
assigned yet, as they will be overridden here. id-num's on <id/>'s are
not touched.

If you are not id'ing the entire gloss at once, pass in the return value of
Tag::IndexFunctions::next_id(wf-num, $xml_tagged_string) as the initial $next_id,
otherwise the id numbers will start over again! For example, to id gloss and
example wfs separately, do something like:

    s/(<gloss[^>]*?>)(.+?)(?=<\/def)/$1 . assign_ids_to_wfs($2,1)/e;
    s/(<\/def[^>]*?>)(.+?)(?=<\/gloss)/$1 . assign_ids_to_wfs($2, &next_id("wf-num", $_))/e;
    
=cut
 
=item B<next_id(>I<$attr>, <$xml_tagged_string>B<)>

Takes passed-in xml-tagged text and determines the next id number in
sequence for the indicated attribute. This will be the largest value
found for that attribute plus 1.

=cut

=item B<index_info_formatted_for_sql(>I<$xml_tagged_str>, <$synset_id>, <$which_field>, <$wfi_ref>, <$stag_ref>B<)>

Returns refs to arrays of wf_index and sense_tag info formatted for loading into
sql db.

Parameters:
 
$xml_tagged_str must be a flat ASCII string of wf-tagged text, with id numbers
already assigned. It can be the entire gloss + example sentences, or the gloss only,
or example sentences only.

$synset_id is the sql id for the gloss record.

$which_field can be "examples", "gloss" or undef. If the the passed-in string
is the entire gloss+example sentences, $which_field must be undef, otherwise it
must be either "examples" or "gloss".

$wfi_ref and $stag_ref are array refs for wf_index info and sense_tag info,
respectively. If defined, the routine will push new elements onto the end(s).

Example:      
    my ($wf_index_ref, $sense_tags_ref) = index_info_formatted_for_sql($tagged_gloss, $synset_id);

    foreach my $wfi_rec (@$wf_index_ref) {
        load into sql
    }
    foreach my $stag_rec (@$sense_tags_ref) {
        load into sql
    }
      
=cut

=back

=head1 Tag::Abbreviations Functions

=over 4

=cut

=item B<check_and_tag_Abbreviation>I<$wf>, <$prevword>, <$xml_thus_far>B<)>

=over 4

Sense tags the passed in abbreviation. 
 
Checks whether the passed in word is one of the predefined abbreviations
based on:
   1) whether it is included in the abbreviations file and
   2) whether one or more dependency criteria are matched, if any.

If is an abbreviation, assign type= attribute and rdf=, if a redefinition
value is specified. Determine lemma form for the wf, which will be:
   1) rdf if a redefinition value is specified, otherwise the wf contents.
   2) lemma + % + part of speech, if there are no dependency criteria,
   or if dependency criteria is satisfied.
   3) lemma%pos1|lemma%pos2|... if matched sense keys are for different
   parts of speech.

Finally, sense tag wf to the one (or more) sense keys for which the dependency
criteria is satisfied.

Other parameters:

$prevword is used for checking dependencies, it should contain the
previous token, if any.

$xml_thus_far is the entire gloss/string tagged thus far, and is used
to determine the next id-num to assign when sense-tagging.

=back

=cut

=item B<is_abbreviation>I<$word>B<)>

=over 4

Simple test of whether the word is an abbreviation that does not depend
on the previous word. Returns 1 if it is in the abbreviation file, 0 otherwise.

=back

=cut

