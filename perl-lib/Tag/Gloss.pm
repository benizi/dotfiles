package Tag::Gloss;

use 5.006;
use strict;
use warnings;

require Tag::Classif;
require Tag::Functions;

sub import {
    my @EXPORT = qw(parseGl);
    my $package = shift;
    my ($caller, $file, $line) = caller();

    Tag::Functions->import(@_);
    Tag::Classif->import(@_);
    
    no strict 'refs';
    foreach my $export (@EXPORT) {
        *{"$caller\::$export"} = *{"$package\::$export"};
    }
}

# mask for the termination of a def/ex/aux in the gloss, use as anchor for regex during parsing
my $glterminate = "\\s*(?=\$|\\x22|\\w|\\(|`)";

# double-quoted eg.
my $eg = "\\x22[^\\x22]+?\\x22";

# single-quoted text
my $sqtxt = "\\x60[^\\x27]+?\\x27";

# single or double-quoted text, for example and aux masks
my $quoted = "(?:$sqtxt(?:\\s+or\\s*$sqtxt)*?|$eg(?:\\s+(?:or|rather\\s+than)\\s*$eg)?)";

# mask for an example
my $exmask = "(?:(?:e\\.g\\.,|(?:as|especially)\\s+in)(?:\\s+the\\s+phrase)?\\s*)?$eg";

# mask for books of the Bible
my $bbook = qr/(?:(?:(?:Matthew|Mark|Luke|John|Genesis|Exodus|Leviticus|Deuteronomy|Joshua|Judges|Ruth|Ezra|Nehemiah|Esther|Job|Psalms|Proverbs|Ecclesiast(?:es|icus)|Isaiah|Jeremiah|Lamentations|Eze(?:k|ch)iel|Daniel|Hosea|Joel|Amos|Obadiah|Jonah|Micah|Nahum|Habakkuk|Habacuc|Zephaniah|Haggai|Sophonias|Aggeus|Z[ea]charia[hs]|Malachi(?:as)|Acts(?:\s+of\s+the\s+Apostles)?|Baruch|Tobit|Judith|(?:1|2|II?)\s+(?:Samuel|Kings|Chronicles|Esdras?|Maccabees))\s+[\d:-]*\d)(?:(?:,|\s+(?:and|through))\s+)?)+/;

# masks for aux info that follows (or precedes) a def and represents "ignorable"
# text such as grammatical or usage info. For example, "followed by x", "used
# in the phrase 'x y z'", "usually preceded by x", etc. handled by
# $auxquote; and varieties of "often used in combination", handled by $combine). 
my $combine = "(?:(?:(?:(?:$eg|$sqtxt)\\s+is|these\\s+words\\s+are)\\s+)?(?:often|usually)\\s+used\\s+(?:in\\s+combination(?:\\s+or\\s+as\\s+[^;\)]*?)?|as(?:\\s+a)?\\s+combining\\s+(?:form|term)s?(?:\\s+(?:as\\s+in|to\\s+indicate)\\s+[^;\)]*?)?))";
my $auxquote = "(?:(?:(?:usually|literally|translates\\s+as|equivalent\\s+to|representing)|(?:(?:(?:usually|often|sometimes)\\s+)?(?:(?:preceded|followed)\\s+by|expressed\\s+as|used\\s+with))|(?:(?:used|especially)\\s+in(?:\\s+the)\\s+phrase)|(?:(?:comparatives?(?:\\s+and\\s+superlative)?|superlative)\\s+of)|(?:from\\s+the\\s+\\w+(?:\\s+word)?\\s+for)|abbreviated|(?:meaning\\s+literally)|(?:usually\\s+follows)|(?:with(?:\\s+verb)?)|(?:all\\s+)?used\\s+chiefly\\s+with(?:\\s+qualifiers))\\s*$quoted(?:[^\)]*?))";
my $quoteaux = "(?:$quoted\\s+(?:is|comes\\s+from)\\s+[^\)]+?)";
my $miscpaux = "(?:(?:when\\s+(?:capitalized|used\\s+broadly))|(?:extended\\s+sense)|postpositive|(?:(?:always|sometimes)\\s+used\\s+|contrastive|degree\\s+adverb|intensifier|quantifier|sentence|archaic|literally|(?:often|usually|frequently)\\s+(?:(?:in\\s+the\\s+)?plural|used|in\\s+combinations?)\\b|used\\s+(?:as|in|for|with|ironically|only)\\b|the\\s+feminine\\s+of|nontechnical|prefatory|literal\\s+meanings?|from\\s+a\\s+combination\\s+of\\b|from\\s+(?:the\\s+)?(?:Sanskrit|French|Latin|Cockney)\\b|from\\s+$bbook)(?:[^\)]*?))";
my $miscunpaux = "(?:(?:(?:always|sometimes|usually|often)\\s+)?used\\s+(?:with\\s+(?:comparisons|either\\s+mass\\b|a\\s+negative|a\\s+form\\s+of\\s+negation)|as\\s+(?:(?:a|an|in)\\s+)?(?:intensifiers?|combining\\s+(?:form|term)s?|combination)|in\\s+(?:(?:the\\s+)?(?:plural|negative)|compounds|combinations?(?!\\s+with))|for\\s+emphasis|informally)(?:[^\)]*?))";

# masks for def examples (double-quoted text preceded by certain fixed phrases)
# $defeg1 = as in e.g. "...."; e.g. "..."; used/as in the expression/phrase "...";
# usually/especially in the phrase "..."; also "..."; [found at the end of the def]
# $defeg2 = --as "...."--; --his "..." [found in middle or end of def]
my $defeg1 = "(?:(?:as\\s+in(?:\\s+the\\s+expression|\\s+e\\.g\\.\\,?)?\\s*$eg)|(?:(?:(?:now\\s+)?used(?:\\s+only\\s+in|\\s+in\\s+the\\s+plural)?|esp(?:ecially|\\.)?|usually)\\s+in\\s+(?:the\\s+)?(?:expression|phrase|such\\s+phrases\\sas)\\s*$eg(?:or\\s*$eg)?)|(?:(?:e\\.g\\.|also|for\\s+example)\\s*$eg));";
my $defeg2 = "\\-\\-(?:as|his)\\s*$eg(?:\\-\\-|;)";

# masks for dates--year ranges, 15(th)-16th century, and "from 17th to 18th centuries" 
my $numth = "\\d+(?:th|st|[rn]d)";
my $drange = "(?:c(?:irca|a?\\.)\\s+)?[0-9-]+|(?:$numth|\\d+)(?:-$numth)?\\s+[cC]entury|from\\s+$numth\\s+to\\s+$numth\\s+[cC]enturies";

# Auxiliary parentheticals inside def (not being tagged)
#my $questionable = qr/(?:used\s+(?:of|by|for|on|when|to|after)|used?\s+only\s+in|often|probably|mostly|sometimes|typically|resembl(?:es|ing)|see|(?:similar|related)\s+to|such\s+as)\s+/;
my $parenthetical = qr/(?:used\s+(?:to|after)|used?\s+only\s+in|probably)\s+/;

# Chemical symbols
my $decimal = "(?:\\.\\d+)";
my $chem_elt = "(?-i:A[cglmrstu]|B[aehikr]?|C[adeflmorsu]?|D[bsy]|E[rsu]|F[emr]?|G[ade]|H[efgos]?|I[nr]?|Kr?|L[airu]|M[dgnot]|N[abdeiop]?|Os?|P[abdmortu]?|R[abefhnu]|S[bcegimnr]?|T[abcehilm]|Uu[bhopqstu]|[VW]|Xe|Yb?|Z[nr])";
my $chem_in_paren = "(?:(?:[\\[(-]?(?:$chem_elt|n)(?:\\d+(?:$decimal)?)?(?:[:=-])?(?:[\\])]\\d*)?)+-?)";

# Auxiliary definitional material
my $auxdef = qr/(?:(?:(?:used\s+)?(?:(?:espe?cially|chiefly|mainly|primarily)\s+)?of\s+)|(?:(?:usually|often|espe?cially|normally|primarily|frequently|widely)(?:used|for)))[^)]+?/;

sub _tagIntra {
    my ($def, $pos) = @_;
    # Tag intra-def examples
    unless ($def =~ s|(<def>.+?)($defeg1\s*)(</def>)|_tagIntra($1, $pos) . $3 . Tag::Functions::tag_elt($2, "ex")|eo) {
        pos($def) = 0;
        $def =~ s/($defeg2|$eg)/Tag::Functions::tag_elt($1, "ex")/geo;
    }
    # Rein in stray punc following </ex>
    $def =~ s/(<\/ex>)([;,\.!?])/$2$1/go;
    my $ret = "";
    {
        $def =~ s/^<(ex|classif|aux).+?<\/\1>\s*//o && do {$ret .= $&; redo;};
        $def =~ s/^<\/?(def|qf)[^>]*>//o && do {$ret .= $&; redo;};
        # Tag books of the bible
        $def =~ s/^\($bbook\)//o && do {
            $ret .= Tag::Functions::tag_elt($&, "aux", {tag => "ignore"});
            redo;};
        # Tag (= Bacchus), (= joules/second), etc.
        $def =~ s/^\(=[^\)\(<]+?\)//o && do {
            $ret .= Tag::Functions::tag_elt($&, "aux", {tag => "ignore"});
            redo;};
        # Tag intra-def parentheticals...
        $def =~ s/^\(${parenthetical}[^\)\(<]+?\)//o && do {
            $ret .= Tag::Functions::tag_elt($&, "aux", {tag => "ignore"});
            redo;};
        # Tag intra-def <arg>'s here (parentheticals in verb synsets only)
        $pos =~ /^v/oi && $def =~ s/^\([^\)\(<]+?\)//o && do {
            $ret .= Tag::Functions::tag_elt($&, "aux", {type => "arg"});
            redo;};
        $def =~ s/^[^\(]+?(?=$|\()//o && do {$ret .= $&; redo;};
        $def =~ s/^\([^\(\)]+?\)//o && do {$ret .= $&; redo;};        
        $def =~ s/^\([^\(\)]+?(?=$|\()//o && do {$ret .= $&; redo;};        
    }
    print STDERR "LEFT UNTESTED [_tagIntra]:\n$def\n\n" if $def ne "";
    $ret;
}

sub parseGl {
    my ($gl, $pos) = @_;
    $pos ||= "";
    my $class_re = classif_regex();

    my $newgl = "";         # Tagged version
    my $chars_hidden = 0;   # XML-unfriendly chars in gloss; hide for tokenization
    
    # Move (chiefly x) or (chiefly x y) or (domain class) to start of gloss
    $gl =~ s/^([^\(].+?)\s+(\((?:(?:chiefly(?:\s[^\s\)]+?){1,2})|$class_re)\))\s*$/$2$1/o;

    $gl =~ s/\s+$//o;
    $gl =~ s/n`t/n't/go;
    $gl =~ s/(\d)(inch|p\.m\.)/$1 $2/go;
    
    # Suck out extra spaces
    $gl =~ s/;\s{2,}/;\ /go;

    # Get rid of extra ; (these shouldn't exist)
    $gl =~ s/;\s+;/;/go;

    # Insert space between word and parenthesized YYYY or YYYY range
    $gl =~ s/(\w)(\([12]\d{3})/$1 $2/go;

    # Replace any _'s with spaces
    $gl =~ s/_/ /go;

    # Close up space following open paren (but only if at start of string
    # or following a space!)
    $gl =~ s/((?:^|\ )\()\ +(\w)/$1$2/go;

    # Insert semicolon if none at end of gloss
    $gl =~ s/([^;:])$/$1;/o;

    if ( $gl =~ /[<>&]/o ) {
        $chars_hidden = 1;
        $gl = Tag::Functions::Gloss_prep($gl);
    }

    # Insert spaces after commas between sequences of digits, eg.
    # 1,2,3,4,5,... and 1,5,10,15,etc...
    $gl =~ s/((?:(?:,(?:\d{1,2}|(?:\d{4,}))){2,})[,\d]+?(?=$|[^,\d]))/_insp($1)/geo;

GLOSSTAG:
    {
        $gl =~ /^ *$/ && do {last;};
        
        # paren'd classification info preceding def
        !$newgl && $gl =~ s/^\(\ *((?:$class_re)(?:(?:\ +and|,)\ +$class_re)*)\ *\)\s*(?=\w|\")//io && do {
            # Just do outer <classif>...</classif> tagging here.
            my ($lemma, $sk, $type) = classif_tag_info($1);
            $newgl .= Tag::Functions::tag_elt("($1)", "classif", {type => $type});
            redo GLOSSTAG;};

        # Classification info not paren'd preceding def, tag as <classif>
        !$newgl && $gl =~ s/^($class_re):\s*(?=\w|\")//io && do {
            # Just do outer <classif>...</classif> tagging here.
            my ($lemma, $sk, $type) = classif_tag_info($1);
            $newgl .= Tag::Functions::tag_elt("($1)", "classif", {type => $type});
            redo GLOSSTAG;};

        # Other info preceding def followed by a colon that is probably
        # classification info. Tag as <classif type="unknown"> if no classif
        # info found, otherwise let pass through
        $newgl !~ /<(?:classif|aux|def)/o && $gl =~ s/^([A-Z][^:;]+):\s+//o && do {
            $newgl .= Tag::Functions::tag_elt("($1)", "classif", {type => "unk"});
            redo GLOSSTAG;};

        # ignorable aux (paren'd info) preceding def, including books of the Bible
        $gl =~ s/^\((?:$auxquote|$quoteaux|$miscpaux|$bbook)\)\s*(?=\w|\")//o && do {
            $newgl .= Tag::Functions::tag_elt($&, "aux", {tag => "ignore"});
            redo GLOSSTAG;};

        # All other (paren'd info) preceding def, except for chem symbols,
        # dates/date ranges, and aux def info (to be picked up by Ignore.pm)
        # Tag as <aux type="ignore"> if no classif info found, otherwise let
        # pass through.
        $newgl !~ /<(?:classif|aux|def)/o && $gl !~ /^\((?:$drange|$chem_in_paren|$auxdef)\)\s*(?=\w|\")/o && $gl =~ s/^\([^\(\)]+?\)\s*(?=\w|\")//o && do {
            $newgl .= Tag::Functions::tag_elt($&, "aux", {tag => "ignore"});
            redo GLOSSTAG;};

        # entirely (paren'd gloss) [ignore for tagging]
        $gl =~ s/^\([^\)]+?\);$glterminate//o && do {
            $newgl .= Tag::Functions::tag_elt($&, "aux", {tag => "ignore"});
            redo GLOSSTAG;};

        # def followed by parenthetical that contains auxiliary info
        # with quoted text
        $gl =~ s/^([^\"][^;\(]+?\s*)(\((?:$auxquote|$combine|$quoteaux|$miscpaux)\);$glterminate)//o && do {
            $newgl .= Tag::Functions::tag_elt($1, "def") . Tag::Functions::tag_elt($2, "aux", {tag => "ignore"});
            redo GLOSSTAG;};

        # def followed by parenthetical containing example sentence(s)
        $gl =~ s/^([^\"][^;\(]+?\s*)(\((?:$exmask|$combine)(;\s*$eg)*\);$glterminate)//o && do {
            $newgl .= Tag::Functions::tag_elt($1, "def") . Tag::Functions::tag_elt($2, "ex");
            redo GLOSSTAG; };

        # Unparenthesized aux info, NOT preceded by a def (but what
        # follows looks like a def)
        # followed by "x"; usually preceded by `x' or `y' or `z'; etc.)
        $gl =~ s/^\ *((?:$auxquote|$combine|$miscunpaux|(?:$quoteaux(?:;\ *)?)+);$glterminate)(?=\ *[^\"`])//o && do {
            $newgl .= Tag::Functions::tag_elt($1, "aux", {tag => "ignore"});
            redo GLOSSTAG;};

        # Unparenthesized aux info, NOT preceded by a def, but there is
        # an existing def already
        # followed by "x"; usually preceded by `x' or `y' or `z'; etc.)
        $newgl =~ /<def/ && $gl =~ s/^\ *((?:$auxquote|$combine|$miscunpaux|(?:$quoteaux(?:;\ *)?)+);$glterminate)//o && do {
            $newgl .= Tag::Functions::tag_elt($1, "aux", {tag => "ignore"});
            redo GLOSSTAG;};

        # def followed by unparenthesized auxiliary info, e.g.,
        # (followed by "x", usually preceded by `x' or `y', etc.)
        $gl =~ s/^([^\"][^;\(]+?;\s*)((?:$auxquote|$combine|$miscunpaux|(?:$quoteaux(?:;\ *)?)+);$glterminate)//o && do {
            $newgl .= Tag::Functions::tag_elt($1, "def") . Tag::Functions::tag_elt($2, "aux", {tag => "ignore"});
            redo GLOSSTAG;};

        # def with embedded parens with embedded semis
        $gl =~ s/^[^\"][^;\(]+?(\([^\)]*\)[^;\(]*?)+;$glterminate//o && do {
            $newgl .= Tag::Functions::tag_elt($&, "def");
            redo GLOSSTAG;};

        # def followed by : or , followed by example sentence
        $gl =~ s/^([^\"][^;:]+?[:,]\s*)($exmask;$glterminate)//o && do {
            $newgl .= Tag::Functions::tag_elt($1, "def") . Tag::Functions::tag_elt($2, "ex");
            redo GLOSSTAG;};

        # plain-vanilla def
        $gl =~ s/^[^\"][^;]+?;$glterminate//o && do {
            $newgl .= Tag::Functions::tag_elt($&, "def");
            redo GLOSSTAG;};

        # "..."-Jane Austen example sentence
        $gl =~ s/^$exmask\s*-+\s*[^\";]+?;$glterminate//o && do {
            $newgl .= Tag::Functions::tag_elt($&, "ex");
            redo GLOSSTAG;};

        # "..." example sentence followed by text such as
        # ', etc.' or ', in baseball' or ', like a x' or ', as of x'
        $gl =~ s/^$exmask,\s\w[^;]+?;$glterminate//o && do {
            $newgl .= Tag::Functions::tag_elt($&, "ex");
            redo GLOSSTAG;};

        # "..." example sentence followed by (parenthetical material)
        $gl =~ s/^$exmask\s+\([^\)]+?\);$glterminate//o && do {
            $newgl .= Tag::Functions::tag_elt($&, "ex");
            redo GLOSSTAG;};

        # "..." or "..." example sentence
        $gl =~ s/^($exmask\s?or\s?$exmask)+[;,]$glterminate//o && do {
            $newgl .= Tag::Functions::tag_elt($&, "ex");
            redo GLOSSTAG; };

        # "..." example sentence (may be delimited by , as well as ;)
        $gl =~ s/^$exmask[;,]$glterminate//o && do {
            $newgl .= Tag::Functions::tag_elt($&, "ex");
            redo GLOSSTAG; };

        # Leftovers
        $gl =~ s/^\".+?$//o && do { $newgl .= Tag::Functions::tag_elt($&, "ex"); redo GLOSSTAG; };
        $gl =~ s/^.+?$//o && do { $newgl .= Tag::Functions::tag_elt($&, "def"); redo GLOSSTAG; };
    }
    
    # Merge serial defs, which occurs because the semicolon is
    # ambiguous--it is used to delimit defs and also as punctuation.
    $newgl =~ s|</def><def>||go;
    
    # Move any <aux>-tagged stuff that is outside the def in front of exs
    while ( $newgl =~ s|(<ex.+?</ex>)(<aux.+?</aux>)|$2$1|go ) {};

    # Tag <ex> and <aux> within defs
    $newgl =~ s|<def>[^<]+?</def>|_tagIntra($&, $pos)|geo;

    # Move any embedded examples outside </def>
    while ( $newgl =~ s|(<ex.+?</ex>[; ]*)(</def>)|$2$1|go ) {};
    
    # Replace hidden XML-unfriendly chars with entities
    $newgl = Tag::Functions::Gloss_post($newgl) if $chars_hidden;

    return $newgl;
}

sub _insp {
    my $str = shift;
    $str =~ s/,/, /go;
    $str;
}

1;

=head1 NAME

Tag::Gloss -- Everything you've ever needed to parse a gloss.

=head1 Functions

=over 4

=item B<parseGl(>I<text>, <pos>B<)>

Takes as input an untagged, unparsed gloss, and returns it tagged into its
constituent parts: minimally, the definition in <def>...</def>, but also any
auxiliary info (<aux>...</aux>) and example sentences (in individual
<ex>...</ex> tags).

Definitions may have embedded example sentences, the internal function
B<_tagIntra> finds and tags these. It also XML-tags certain kinds of
parentheticals within defs that are not handled by Ignore.pm. 

Side effects: if calling routine does not handle XML-unfriendly chars by
hiding, then parseGl will replace them with entities.

