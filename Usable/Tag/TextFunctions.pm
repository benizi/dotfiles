package Tag::TextFunctions;

use 5.006;
use strict;
use warnings;

use WordNet;
use WordNet::Morphology;
require Tag::IndexFunctions;

my @defaults = ( morphdir  => '/wordnet/wn/2k3/lib/perl/various',
                 case_sensitive => 1);

my ($wn, $morph);

sub import {
    my @EXPORT = qw(&XML_prep &XML_post &Gloss_prep &Gloss_post &Gloss_safety_measure 
       &Hide_apos &Unhide_apos &Hide_comma &Unhide_comma
       &Hide_parens &Unhide_parens &Hide_quest &Unhide_quest
       &hasAttributeValue &getAttributeValue &setAttributeValues
       &deleteAttribute &assign_potential_lemmas &assign_pseudolemma &zpad
       &sense_tag_tf &build_mwf_lemma &tag_elt &tag_elt_as_punc &format_XML_contents_tf
       &wrap_synset_elts &get_lemmas_by_moaning &decontract);
    my $package = shift;
    my ($caller, $file, $line) = caller();

    $wn = WordNet->new(@defaults, @_);
    $morph = WordNet::Morphology->new(@defaults, @_);
    
    Tag::Abbreviations->import(@_);

    no strict 'refs';
    foreach my $export (@EXPORT) {
        $export =~ s/^&//;
        *{"$caller\::$export"} = *{"$package\::$export"};
    }
}

my (%xml_subs,
    %gloss_subs, %gloss_undo, %gloss_post,
    %apos_subs,
    %number_subs,
    %paren_subs,
    %quest_subs);

%xml_subs = ('&', '&amp;',
             '<', '&lt;',
             '>', '&gt;');

# Only USABLE (don't appear in WN data.*) UTF-8 compatible characters
# are \x09 (== tab) \x0D (== Macintosh Line-break?)
# and \x7F (== unknown -- looks like a box in a Linux terminal)
# but, we have several characters to worry about:
# <, >, & for starters, and ' and ` later

my ($X, $Y, $Z) = ("\x09", "\x7f", "\x0d");

%gloss_subs =  ('<', "$X$Y$Y$Y",
        '>', "$X$Y$Y$Z",
        '&', "$X$Y$Z$Y");
%apos_subs =   ("'", "$X$Y$Z$Z");
%number_subs = (',', "$X$Z$Y$Y",
        ':', "$X$Z$Y$Z");
%paren_subs =  ('(', "$X$Z$Z$Y",
        ')', "$X$Z$Z$Z");
%quest_subs =  ('?', "$X$Y$Y$X");

# other available codes: XYZX, XZYX, XZZX, XYX, XZX

# Undoes the mapping of funky replacement characters to entities
# (in other words, maps from &amp; to \x09\x7f\x0d\x7f)
%gloss_undo = map { ($xml_subs{$_}, $gloss_subs{$_}); } keys %xml_subs;

# Maps from funky replacement chars to entities.
%gloss_post = reverse %gloss_undo;

sub convert_to_speedy {
    my ($href, $aref, $reva) = @_;
    @$aref = map { (qr{\Q$_\E}, $href->{$_}); } keys %$href;
    return if not defined $reva;
    @$reva = map { (qr{\Q$href->{$_}\E}, $_); } keys %$href;
}

my ($xmla, $xmlr,
    $glossa, $glossr, $glossu,
    $aposa, $aposr,
    $numbera, $numberr,
    $parena, $parenr,
    $questa, $questr) =
    ([], [], [], [], [],
     [], [], [], [], [],
     [], [], []);

convert_to_speedy(\%xml_subs,     $xmla,    $xmlr);
convert_to_speedy(\%gloss_subs,   $glossa);
convert_to_speedy(\%gloss_post,   $glossr);
convert_to_speedy(\%gloss_undo, $glossu);
convert_to_speedy(\%apos_subs,    $aposa,   $aposr);
convert_to_speedy(\%number_subs,  $numbera, $numberr);
convert_to_speedy(\%paren_subs,   $parena,  $parenr);
convert_to_speedy(\%quest_subs,   $questa,  $questr);

# not exported. Basis for most of the replacement functions in here
sub do_subs {
    my ($aref, $string) = @_;
    for (my $i=0; $i<@$aref; $i+=2) {
        $string =~ s/$aref->[$i]/$aref->[$i+1]/g;
    }
    return $string;
}

sub XML_prep             { return do_subs($xmla,    @_); }
sub XML_post             { return do_subs($xmlr,    @_); }
sub Gloss_prep           { return do_subs($glossa,  @_); }
sub Gloss_post           { return do_subs($glossr,  @_); }
sub Gloss_safety_measure { return do_subs($glossu,  @_); }
sub Hide_apos            { return do_subs($aposa,   @_); }
sub Unhide_apos          { return do_subs($aposr,   @_); }
sub Hide_comma           { return do_subs($numbera, @_); }
sub Unhide_comma         { return do_subs($numberr, @_); }
sub Hide_parens          { return do_subs($parena,  @_); }
sub Unhide_parens        { return do_subs($parenr,  @_); }
sub Hide_quest           { return do_subs($questa,  @_); }
sub Unhide_quest         { return do_subs($questr,  @_); }
    
#=item B<tag_wf(>I<text>, I<cmd>, I<key>, I<%extra>B<)>

#Returns $text surrounded by a <wf cmd="$cmd" key="$key"></wf> tag.

#Extras can contain key-value pairs for more attributes for the wf tag.

#=cut

sub zpad {
    my ($num, $len) = @_;
    return undef if $len <= 0;
    return $num if length($num) > $len;
    return substr("0" x $len . $num, $len * -1);
}

sub hasAttributeValue {
    my ($attr, $value, $elt) = @_;
    return 0 if !defined($value) || !defined($attr);
    $value ||= '[^"]*?';
    my $ret = ($elt =~ /^\s*<[^\s\/<>]+?\b[^<>\/]*?\s$attr="(?:$value)"/) ? 1 : 0;
    return $ret;
}

sub getAttributeValue {
    my ($attr, $elt) = @_;
    return undef if !defined($attr) or $elt !~ /^\s*<[^\s\/<>]+?\b[^<>\/]*?\s$attr="/;
    my $ret = ($elt =~ /^\s*<[^\s\/<>]+?\b[^<>\/]*?\s$attr="([^"]+?)"/) ? $1 : "";
    return $ret;
}

sub setAttributeValues {
    my ($elt, $attrs) = @_;
    return $elt if !defined($attrs);
    foreach my $key (keys %$attrs) {
        next if !defined($$attrs{$key});
        $elt = deleteAttribute($key, $elt);
        $elt =~ s/^(\s*<[^\s\/<>]+?\b)/$1 $key="$$attrs{$key}"/;
    }
    return $elt;
}

sub deleteAttribute {
    my ($attr, $elt) = @_;
    return $elt if !defined($attr);
    $elt =~ s/^(\s*<[^\s\/<>]+?\b[^<>\/]*?)\s$attr=\"([^\"]*?)\"/$1/;
    return $elt;
}

sub assign_potential_lemmas {
    my ($wf, $special_handling, $rdf) = @_;
    $special_handling ||= 0;
    $rdf ||= getAttributeValue('rdf', $wf);
#xxx    
#    unless ( hasAttributeValue('lemma', "", $wf) ) {
    
        $wf = deleteAttribute('lemma', $wf);
        
        my $word;
        my %attrs;
        if ( defined($rdf) && $rdf ne "" ) {
            $attrs{rdf} = $rdf;
            $word = $rdf;
        } else {
            $wf =~ /^\s*<wf[^<>]*>(.+?)<\/wf>/;
            $word = $1;
            # Remove trailing punctuation (except period) in preparation
            # for lemmatization (but only strip if does not follow
            # final punctuation other than a period!) 
            $word =~ s/([^:;,!\?\ ])[:;,!\?]\ *$/$1/g;
        }

        my $lemma;
        if ( my $acronym = _acronym($word) ) {
            # Looks as if word could be an acronym, so attempt a
            # case-sensitive wn lookup. If found, build lemma value as
            # $word%1 (since always a noun) and leave trailing period.
            if ( $wn->is_word($acronym, "n") ) {
                $lemma = "$acronym%1";
                $attrs{type} = "acronym";
            }
        }

        unless ( defined($lemma) ) {
            $lemma = get_lemmas_by_moaning($word, $special_handling);
        }
        $attrs{'lemma'} = $lemma;
        return setAttributeValues($wf, \%attrs);
        
#    }
#    return $wf;
}

sub get_lemmas_by_moaning {
    my ($word, $special_handling) = @_;
    $special_handling ||= 0;
    my $lemma;
    my %pos_map = qw/n 1 v 2 a 3 r 4/;

    $word =~ s/\.\ *$// if $word !~ /[\ _]/;
    # Call moan for potential lemmas/pos of form as is (case-sensitively)
    my @word_pos_pairs = $morph->moan($word);
    if ( $special_handling && $word =~ /^[A-Z]/ && (my @word_pos_pairs_lc = $morph->moan(lcfirst $word)) ) {
        # Append any additional potential lemmas/pos for words that are
        # the first word in a sentence
        @word_pos_pairs = map {$_} @word_pos_pairs, @word_pos_pairs_lc; 
    }

    if (!@word_pos_pairs) {
        $lemma = $word;
        $lemma =~ s/\'s$|(?<=s)\'$//;
    } else {
        # build $lemma="baseform1%pos1|baseform2%pos2|..."
        while (@word_pos_pairs) {
            my ($form, $ben_pos) = splice @word_pos_pairs, 0, 2;
            my $pos = $pos_map{substr $ben_pos, 0, 1};
            unless ( defined($lemma) && ( $lemma =~ /(?:^|\|)$form%$pos(?=$|\|)/ ) ) {
                $lemma = !defined($lemma) ? "$form%$pos" : "$lemma\|$form%$pos";
            }
        }
    }
    return $lemma;
}

sub assign_pseudolemma {
    my $wf = shift;
    $wf =~ s{<wf.+?</wf>}{deleteAttribute('lemma', $&)}ge;
    $wf =~ s/(<wf[^<>]*)(>(?:<id[^<>]*>)*(.+?))(?=<\/wf>)/$1 lemma="$3"$2/g;
    return $wf;
}

sub build_mwf_lemma {
    my $mwf = shift;
    my $mwf_lemma = format_XML_contents_tf($mwf);
    $mwf_lemma =~ s/ /_/g;
    return $mwf_lemma;
}

sub sense_tag_tf {
    my ($elt, $lemma_sk_pairs, $tag, $xml_str) = @_;
    return if !defined($elt);
    die "\$lemma_sk_pairs not defined in sense_tag_tf!" if !defined($lemma_sk_pairs);
    die "\$tag not defined!" if !defined($tag);
    die "\$xml_str not defined!" if !defined($xml_str);

    # Start with the next id-num in sequence...
    my $id_num = Tag::IndexFunctions::next_id('id-num', $xml_str);
    
    my $wf_lemma;
    my $cnt;
    while (@$lemma_sk_pairs) {
        my $lemma = shift @$lemma_sk_pairs;
        my $sk = shift @$lemma_sk_pairs;
        my $id = "<id/>";
        $id = setAttributeValues($id, {'id-num' => $id_num, 'sk' => $sk, 'lemma' => $lemma});
        $elt =~ s/^\s*<[^<>\/]+?>/$&$id/;

        if ( !defined($wf_lemma) ) {
            $wf_lemma = $lemma;
        } elsif ( $wf_lemma !~ /(?:^|\|)$lemma(?:[\|%]|$)/ ) {
            $wf_lemma = join("|", $wf_lemma, $lemma);
        }

        $id_num++;
        $cnt++;
    }
    $tag = "un" if $cnt > 1;    
    return Tag::TextFunctions::setAttributeValues($elt, {lemma => $wf_lemma, tag => $tag});
}

sub format_XML_contents_tf {
    my ($str, $use_lemmas) = @_;
    $use_lemmas ||= 0;
    my $ppstr;
    
    my $sep = " ";
    while ( $str =~ m/<wf[^<>]*>(.+?)<\/wf>/g ) {
        my $wf = $&;
        my $contents = $1;
        $contents =~ s/<id.+?\/>//g;
        my $lemma;
        if ( $use_lemmas && (my $attval = getAttributeValue('lemma', $wf)) ) {
            my %lemmas;
            $attval =~ s/^(.+?)(?=$|%\d+)/$lemmas{$1}++/ge;
            foreach my $l (keys %lemmas) {
                $lemma = defined($lemma) ? "$lemma\|$l" : $l;
            }
        }
        $lemma ||= $contents;
        # The following is not really necessary anymore, since all 
        # type="punc" wfs should have sep="" attribute, but may as 
        # well keep in to be on the safe side
        $sep = "" if hasAttributeValue('type', "punc", $wf) && $wf =~ />[:;,!\.\?\)]/;
        $ppstr = !defined($ppstr) ? $lemma : "$ppstr$sep$lemma";
        $sep = hasAttributeValue('sep', "", $wf) ? getAttributeValue('sep', $wf) : " ";
    }
    
    return $ppstr;
}

sub _strip_pos {
    my $tmp = shift;
    $tmp =~ s/%\d+?$//;
    return $tmp;
}

sub tag_elt {
    my ($contents, $tag, $attrs) = @_;
    if ( !defined($attrs) ) {
        return "<$tag>$contents</$tag>";
    } else {       
        return setAttributeValues("<$tag>$contents</$tag>", $attrs);
    }
}

sub tag_elt_as_punc {
    my ($contents, $attrs) = @_;
    $$attrs{type} = "punc";
    $$attrs{tag} = "ignore";
    return tag_elt($contents, "wf", $attrs);
}

sub wrap_synset_elts {
    my ($gloss, $examples) = @_;
    return undef if (!defined($gloss) || $gloss eq "") && (!defined($examples) || $examples eq "");
    my $synset;
    $synset .= tag_elt($gloss, "gloss") unless !defined($gloss) || $gloss eq "";
    $synset .= tag_elt($examples, "examples") unless !defined($examples) || $examples eq "";        
    return tag_elt($synset, "synset"); 
}

# Tests (possibly inflected) input wf for acronym. Returns the
# (simplistically) uninflected version of the form if it is
# an acronym, and undef otherwise.
sub _acronym {
    my $form = shift;
    return undef if $form =~ /^(?:X?(?:I{2,3}|IV|VI{1,3}|IX))|X[IV]$/;
    if ($form =~ /^((?:[A-Z]\.?){2,})(?:\'[sd])?$/) {
        return $1;
    } elsif ($form =~ /^((?:[A-Z]){2,})s$/) {
        return $1;
    }
}

my %contractions = ();
{
    $contractions{"I'm"}   = [    "I",  "I",  "be",  "'m" ];
    $contractions{"ain't"} = [   "be", "ai", "not", "n't" ];
    $contractions{"can't"} = [  "can", "ca", "not", "n't" ];
    $contractions{"won't"} = [ "will", "wo", "not", "n't" ];
    for my $word (qw/I it they we you he she/) {
        $contractions{$word."'d"}     = [ $word, $word,
                                          "have|do|would|should", "'d" ];
        $contractions{$word."'ll"}    = [ $word, $word, "will", "'ll" ];
        $contractions{$word."'ll've"} =
            [ $word, $word, "will", "'ll", "have", "'ve" ];
    }
    for my $word (qw/they we you/) {
        $contractions{$word."'re"}    = [ $word, $word, "be", "'re" ];
    }
    for my $word (qw/I they we you/) {
        $contractions{$word."'ve"}    = [ $word, $word, "have", "'ve" ];
        $contractions{$word."'d've"}  =
            [ $word, $word, "would", "'d", "have", "'ve" ];
    }
    for my $word (qw/are could dare do does did had has have
                  is must need should was were would/) {
        $contractions{$word."n't"}    = [ $word, $word, "not", "n't" ];
    }
    for my $word (qw/he here it she that there what where/) {
        $contractions{$word."'s"}     = [ $word, $word, "is", "'s" ];
    }
    $contractions{"let's"}    = [ "let", "let", "us", "'s" ];
    $contractions{"oughtn't"} = [ "ought", "ought", "not", "n't" ];

    foreach my $key (keys %contractions) {
        next unless lc $key ne $key;
        $contractions{lc($key)} = $contractions{$key};
    }
}
my %general_ending = ();
{
    $general_ending{"'d"}  = [ "have|do|would|should", "'d" ];
    $general_ending{"'ll"} = [ "will", "'ll" ];
    $general_ending{"'re"} = [ "be",  "'re" ];
}

sub decontract {
    local $_ = shift;
    return () unless /\'/;
    return () if /^(.*)\'e?d$/ and is_abbreviation($1);
    my $lc = lc;
    return _matched_case($_, $contractions{$lc}) if defined $contractions{$lc};
    foreach my $e (keys %general_ending) {
        if (my ($base) = (/^(.*)$e$/)) {
            return ($base, $base, @{$general_ending{$e}});
        }
    }
    ();
}

sub _matched_case {
    my ($o, $aref) = @_;
    my @ret = @$aref;
    my $uc = lc $o eq $o ? 0 : 1;
    if ($uc) { $ret[1] = join "|", map { ucfirst $_; } split /\|/, $ret[1]; }
    @ret;
}

1;

__END__
   
=head1 NAME

Tag::TextFunctions

=over 4

Some useful text utilities for Tag::etcetera purposes

=back

=head1 Functions

=over 4

=cut

=item B<XML_prep(>I<text>B<)>

Returns text after replacing some troublesome punctuation symbols with entities

=item B<XML_post(>I<text>B<)>

Undoes what XML_prep does

=item B<Gloss_prep(>I<text>B<)>

Returns text after replacing some troublesome punctuation with XML::LibXML-friendly characters

=item B<Gloss_post(>I<text>B<)>

Undoes what Gloss_prep does

=item B<Hide_apos(>I<text>B<)>

Returns text after replacing apostrophes with XML::LibXML-friendly characters

=item B<Unhide_apos(>I<text>B<)>

Undoes what Hide_apos does

=item B<Hide_comma(>I<text>B<)>

Returns text after replacing commas with XML::LibXML-friendly characters

=item B<Unhide_comma(>I<text>B<)>

Undoes what Hide_comma does

=cut

=item B<zpad(>I<num>, <length>B<)>

Pad $num with leading zeros to specified length string. E.g., zpad(35, 6) will return "000035". $num is assumed to be a number, and $len a number greater than zero. The only check performed are that $len is >= 0 and the length of $num is <= $len.

=cut

=back

=head1 XML-ish Functions

=over 4

=cut

=item B<hasAttributeValue(>I<attribute>, I<value>, I<element>B<)>

Checks for the $value in attribute $attr on the element's start tag, and returns 1 if exists and 0 otherwise. Passing in null for $value will return 1 for any value in $attr, including null.

=item B<getAttributeValue(>I<attribute>, I<element>B<)>

Returns the value of the attribute $attr in element $elt, or undef if no attribute exists. Assumes attribute values to be double-quote delimited.

=item B<setAttributeValues(>I<element>, I<attrs>B<)>

Assigns all attribute/value pairs passed in the hash ref'd by $attrs in the start tag of element $elt. Deletes any previous value of an attribute, if exists. Returns the entire element.

note: Routine allows null value passed in for $attr, so that sep="" can be assigned.

Side effect: overwrites the existing value in the attribute, if one exists.

=item B<deleteAttribute(>I<attribute>, I<element>B<)>

Deletes attribute and attribute value from the element's start tag, if it exists.

=cut

=item B<assign_potential_lemmas(>I<wf>, I<special_handling>, I<rdf>B<)>

If lemma= attribute not already set, determines and assigns all posited lemma/pos pairs as returned from moan. If $special_handling is non-zero, then this is the first word of an example sentence, and so might be capitalized. 

If a redefinition value is passed in, it is used as the word to be lemmatized instead of the tag contents, and rdf= attribute is assigned.

Do not use this routine to assign lemma for ignorable text (which does not get lemmatized), or for abbreviations (which may have trailing punc, and for which sense keys are predetermined)--use assign_pseudolemma for the former and setAttributeValues for the latter instead.

nb. lemma differs from the sense_key form in that case is retained!
    
=cut

=item B<assign_pseudolemma(>I<wf>B<)>

Sets lemma= attribute on the passed-in <wf> (or string of <wf>s) to the contents of each <wf>, without actually lemmatizing the contents. Only to be used for ignorable text, as it does no wn lookup, and thus makes no attempt to determine pos.

Side effect: overrides current lemma=, if any.

=cut

=item B<build_mwf_lemma(>I<mwf>B<)>

Builds lemma based on contents of constituent wfs. Defaults to "_" as concatenator char, unless sep= value is present in the previous wf (in which case that value is used).

note: This will work for mwf's that are either wrapped in <mwf>...</mwf> tags or just a string of wfs not yet tagged as mwf.

=cut

=item B<format_XML_contents_tf(>I<tagged_text>B<)>

Removes XML tags from a tagged string of text and restores spacing based on sep= attribute.

=cut

=item B<sense_tag_tf(>I<(wf|mwf)>, I<lemma_sk_pairs>, I<tag>, I<xml_str>B<)>

Sense tag the passed in wf or mwf. This consists of creating an empty <id/> tag for each sk/lemma pair, setting sk=, lemma=, and id_num= on it, and inserting it into the wf/mwf just after the start tag.

Arguments:

$wf/$mwf is the element to be tagged

$lemma_sk_pairs is an array ref of a list of lemma/sense key tuples

$tag should be either "auto" or "man". If more than one sense_key is being assigned, then $tag gets reset to "un".

$xml_str is the entire XML-tagged gloss. It is used to find the next id-num in sequence.
   
=cut

=item B<tag_elt(>I<contents>, I<tag>, I<$href>B<)>

Wraps $contents in start/end tags. Assigns attributes and their values if passed in as key/value pairs in the hash ref'd by $href.

note: Routine allows for null attribute values, so that sep="" can be assigned.

=cut

=item B<wrap_synset_elts(>I<$gloss>, I<$examples>B<)>

Wraps $gloss in <gloss>...</gloss> tags. If $examples are not null, then wraps them in <examples>...</examples> tags. Finally, wraps the entire thing in <synset>...</synset> tags. The <synset> tag becomes the root gi (=tag) for the Twig object.

=cut

