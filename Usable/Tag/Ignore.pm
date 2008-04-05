package Tag::Ignore;

use 5.006;
use strict;
use warnings;
require Exporter;

use Tag::Stoplist;
use Tag::Functions;

BEGIN {
    our (@ISA, @EXPORT, @EXPORT_OK, %EXPORT_TAGS);

    @ISA = qw(Exporter);
    @EXPORT = ();
    %EXPORT_TAGS = ();
    @EXPORT_OK = qw(&find_and_hideIgnorablePunc &unhideIgnorablePunc &findParentheticals &findIgnores);
}

#
# Regexes for recognizing ignorable classes
#

# Start and end wf-tags, or otherwise word boundary (i.e., space) in untagged contexts
my $wf_start = "(?:\\s*<wf[^<>]*>(?:<id[^<>]*>\\s*)*?(?!<))";
my $wf_end = "(?:<\\/wf>\\s*?)";

# Dates
my $mon = "$wf_start(?:Jan(?:uary|\\.)?|Feb(?:ruary|\\.)?|Mar(?:ch|\\.)?|Apr(?:il|\\.)?|May|June|Jul(?:y|\\.)?|Aug(?:ust|\\.)?|Sept(?:ember|\\.)?|Oct(?:ober|\\.)?|Nov(?:ember|\\.)?|Dec(?:ember|\\.)?)$wf_end";
my $yyyy = "(?:(?:mid\\-)?[12]\\d{3})";
my $yyyy_wf = "$wf_start$yyyy\\-?$wf_end";
my $nyear = "(?:[1-9]\\d{0,2})";
my $yr_range = "(?:$yyyy\\??|$nyear\\??|\\?)[=-](?:$yyyy\\??|$nyear\\??|\\?)";
my $yr_range_wf = "$wf_start$yr_range$wf_end";
my $daymoyr = "(?:(?:$wf_start(?:[1-9]\\d?)$wf_end)?$mon$yyyy_wf)";
my $modayyr = "(?:$mon$wf_start(?:[1-9]\\d?)\\b$wf_end(?:$wf_start,$wf_end)?$yyyy_wf)";
my $moday = "(?:$mon$wf_start(?:[1-9]\\d?)$wf_end)";
my $nth = "$wf_start(?:(?:mid|\\d+(?i:th|st|[rn]d)?)\\-)?\\d+(?i:th|st|[rn]d)$wf_end";
my $circa = "$wf_start(?:c)(?:a?\\.|irca)?$wf_end";
my $bc = "$wf_start(?i:b\\.?c\\.?)$wf_end";
my $ad = "$wf_start(?i:a\\.?d\\.?)$wf_end";
my $century = "$nth$wf_start(?:[Cc]entur(?:y|ies))$wf_end(?:$bc|$ad)?";
my $bc_date = "(?:$wf_start(?:$yyyy|$nyear)\\??\\-?$wf_end(?:$bc|$ad))";
my $ad_date = "(?:(?:$ad$wf_start(?:$yyyy|$nyear)\\??\\-?$wf_end)|(?:$wf_start(?:$yyyy|$nyear)\\??\\-?$wf_end$ad))";
my $allyear = "(?:$century|$yyyy_wf|$wf_start(?:$nyear\\??\\-?)$wf_end)";
my $born_died = "(?:$wf_start(?i:born|died)$wf_end(?:[^\\d\\)]*?$wf_start(?i:in|around)$wf_end)?(?:$ad_date|$bc_date|$allyear))";

my $date = "$bc_date|$ad_date|$allyear|$modayyr|$moday|$daymoyr|$born_died";

# Date ranges
my $century_range = "(?:$nth$wf_start(?:and|to)$wf_end$century)";
my $month_range = "(?:(?:$wf_start\\d+$wf_end)?$mon(?:$wf_start(?:and|to)$wf_end$mon)$yyyy_wf)";
my $date_range = "(?:$ad$yr_range_wf)|(?:$yr_range_wf(?:$bc|$ad))|(?:$century_range)|(?:(?:$bc_date|$ad_date|$allyear|$moday)(?:$wf_start(?:\\-|to)$wf_end)(?:$bc_date|$ad_date|$allyear|$moday))";

# Dates and date ranges with context for ignoring parenthetized dates
my $date_range_in_context = "(?:(?:$circa|(?:$wf_start(?:reigned|active)$wf_end)?$wf_start(?:from)$wf_end)?(?:$date_range|$yr_range_wf))";
my $dates_in_context = "(?:(?:$circa|$wf_start(?:until|in|late)$wf_end)(?:$date))";

# Numbers
my $decimal = "(?:\\.\\d+)";
my $num = "(?:[\\+\\-]?\\d[\\d,]*?(?:$decimal)?)";
my $nums = "(?:\\d's)";
my $nrange = "(?:$wf_start$num\\-$num$wf_end)";
my $frac = "(?:$num/$num)";
my $percent = "(?:$num\%)";
my $cardinals = "(?:" . join("|", &all_cardinals("single")) . ")";
my $prange = "(?:$wf_start(?:$percent|$num)\\-$percent$wf_end)";
# Exponents of the form 10^-52, 10^10, as well as 0.2+5
my $exponent = "(?:(?:0\\.\\d+[\\+\\-]|\\d+\\^[\\+\\-]?)\\d+)";
my $number = "$wf_start(?:$frac|$percent|$exponent|$num|0?$decimal|$cardinals|$nums)$wf_end";
my $nplusfrac = "(?:$wf_start$num$wf_end$wf_start$frac$wf_end)";

# Ranges
my $ncontext = "(?:$wf_start(?:or|and|from|usually|approximately|above|below)$wf_end)(?:(?:$wf_start(?:the)$wf_end)(?:$wf_start(?:digits)$wf_end))?";

# Numeric/percent ranges
my $percent_range = "(?:$wf_start(?:$percent|$num)$wf_end(?:$wf_start(?:\\-|to)$wf_end)$wf_start$percent$wf_end)";
my $numeric_range = "(?:$wf_start$num$wf_end(?:$wf_start(?:\\-|to)$wf_end)$wf_start$num$wf_end)";

# Numeric ranges with context for ignoring parenthesized num ranges
my $nrange_in_context = "(?:$ncontext)?(?:$percent_range|$prange|$numeric_range|$nrange)";

# Other numbers in parenthetical context
my $numconj = "(?:$number(?:(?:(?:$wf_start(?:and|or)$wf_end)$number)+))";
my $num_in_context = "(?:(?:$ncontext)(?:$number|$numconj))|(?:$numconj)";

# Measurements (numbers/ranges followed by units of measure, including
# things like "300 million light years", "# inches/feet/microns in
# diameter", "# feet long". Note that temporal units such as "year(s)",
# "month(s)", "day(s)" are not included as units of measurement, but minutes,
# second(s) and hour(s) are.
my $unit_abbr = "(?:(?:in\\.)|(?:amp|ar|lbs?|c\\.?[cdml]|Ci|dkm|dl|ft|fthm|g|G[bi]|gal|hr|(?i:t)?[Hh]z|[hk]m|[kKMGTPEZY]B|[KMG]|[MKGTEZY]iB|[kMGTPEZY]bit|[kGTPEZY]b|[kMGPEZ]ibit|lbf?|lm?|mA|m\\.?m?|m[gV]|min?|mi?l|mho|MIPS|MFLOP|Mx|nm|ohm|pdl|rad|rbi|rpm|sec|k?V|k?W|Wb|yds?|yrs?)\\.?(?:\\/sec(?:ond)?)*)";
my $units = "(?:$wf_start(?:English)$wf_end)(?:$wf_start(?:f(?:ee)?t\\.?)$wf_end)|(?:(?:$wf_start(?:degrees?)$wf_end)(?:$wf_start(?:[Nn]orth|[Ss]outh)$wf_end))|(?:(?:$wf_start(?:degrees?)$wf_end)?(?:$wf_start(?:C|F|K|[Cc]entigrade|[Cc]elsius|[Ff]ahrenheit|[Kk]elvin)$wf_end))|(?:(?:$wf_start(?:light)$wf_end)(?:$wf_start(?:y(?:ears?|r\\.?))$wf_end)|(?:$wf_start(?:foot)$wf_end)(?:$wf_start(?:candle)$wf_end)|(?:(?:$wf_start(?:square|cu(?:bic)|fluid)$wf_end)?(?:$wf_start(?:pounds?|yards|acres?|(?:f(?:oo|ee)?t\\.?|joules?)(?:\\/sec(?:ond)?)*|gallons?|gram(?:me)?s?|grains|(?:in(?:ch(?:es)?|\\.))|(?:kilo|milli|centi)?meters?|microns?|miles?|hours?|parsecs?|sec(?:ond|par)s?|minutes|hertz|steps\\/min\\.?|pints?|ounces?|drams?|degrees|(?:kilo|kibi|giga|gibi|mega|mebi|tera|tebi|peta|pebi|exa|exbi|zetta|zebi|yotta|yebi)?b(?:yte|it)s?|newtons?|maxwells?|mole|dynes?|lumen|lux|phots|candela|steradian|cycles?)$wf_end)))";
my $meas = "(?:(?:$nplusfrac|$number)(?:$wf_start(?:trillion|[bm]illion|thousand|hundred)$wf_end)?(?:$units|$wf_start$unit_abbr$wf_end))";
my $measure = "(?:$meas+)";
# This regex finds # joules per kelvin per mole, etc.
my $measure_per = "(?:$meas(?:(?:$wf_start(?:per)$wf_end)(?:$units))+)";
my $measure_range = "(?:(?:$percent_range|$prange|$numeric_range|$nrange|$numconj)(?:$units|$wf_start$unit_abbr$wf_end))";

# Measurements and measurement ranges with context for ignoring
# parenthesized measurements, including modifiers such as (20-ft)
# which normally wouldn't be considered ignorable.
my $measure_in_context = "(?:(?:(?:(?:$wf_start(?:equal|up)$wf_end)?$wf_start(?:to)$wf_end)|(?:$wf_start(?:and|or|from|between|usually|above|below)$wf_end)|(?:(?:$wf_start(?:at|or)$wf_end|$circa)?(?:$wf_start(?:approximately)$wf_end)?))?(?:(?:$wf_start$num\-$unit_abbr$wf_end)|(?:(?:$number|$measure)(?:$wf_start(?:and|or)$wf_end(?:$measure))+)|(?:$measure_range|$measure)(?:$wf_start(?:up|ago|high|long|wide|square)$wf_end|$wf_start(?:in)$wf_end$wf_start(?:diameter)$wf_end|$wf_start(?:or)$wf_end$wf_start(?:more)$wf_end)?))";

# Time. Incomplete--it captures 4:00, 12:00 o'clock, 6 a.m., 3 PM, 11p.m.,
# 4-6 pm, but nothing that might look like a book of the bible (2:29-32)
my $clock_digits = "(?:[1-9]|(?:10|11|12))";
my $am_pm = "(?i:a\\.m\\.|p\\.?m\\.?)";
my $oclock = "(?:$clock_digits(?::00))";
my $clock_num = "(?:$oclock|$clock_digits)";
my $clock_num_wf = "$wf_start(?:$clock_num(?:\\-$clock_num)?(?:$am_pm)?)$wf_end";
my $time = "(?:(?:$clock_num_wf)(?:(?:$wf_start(?:o'clock|$am_pm)$wf_end)))";
my $time_in_context = "(?:(?:$wf_start(?:as)$wf_end)(?:$wf_start(?:from)$wf_end)?(?:$time|(?:$wf_start(?:midnight|$clock_num)$wf_end))(?:$wf_start(?:to)$wf_end)(?:$time|(?:$wf_start(?:midnight|$clock_num)$wf_end)))";

# Currency
my $currency = qr/\$$num(?:\/share)?/;
my $million = "(?:$wf_start$currency$wf_end$wf_start(?:million)$wf_end)";
my $currency_in_context = "(?:(?:$wf_start(?:usually)$wf_end)?(?:$million|$wf_start$currency$wf_end))";

# Chemical symbols (all chemical elts except for U(ranium), to avoid
# tagging (US) as a chem symbol!) nb. X is not an element, but is used 
# as a placeholder in glosses. UO captures UO2.
my $chem_elt = "(?-i:A[cglmrstu]|B[aehikr]?|C[adeflmorsu]?|D[bsy]|E[rsu]|F[emr]?|G[ade]|H[efgos]?|I[nr]?|Kr?|L[airu]|M[dgnot]|N[abdeiop]?|Os?|P[abdmortu]?|R[abefhnu]|S[bcegimnr]?|T[abcehilm]|Uu[bhopqstu]|[VW]|Xe|Yb?|Z[nr]|UO)";
my $chem_formula = "(?:(?:(?:$chem_elt)n?)+)\\((?:\\+?X?\\d+n?)+\\)";
my $chem_free = "(?:(?:(?:(?:[R\\-]|$chem_elt)(?:[:=\\d]|$chem_elt)*\\-)|(?:$chem_elt(?:[:=\\d]|$chem_elt)*X?\\d)|(?:\\-(?:$chem_elt)(?:[:=\\d]|$chem_elt)*\\-?))+)";
my $chem_in_paren = "(?:(?:[\\[\\(\\-]?(?:$chem_elt|n)(?:\\d+(?:$decimal)?)?(?:[:=-])?(?:[\\]\\)]\\d*)?)+\\-?)";

# Assorted symbols, including punctuation, that are used qua symbols in
# glosses, and not as punctuation. Includes hidden chars which might be
# used as symbols!
my $hidden_chars = &Gloss_prep('>|<|&|') . &Hide_apos("'|") . &Hide_comma(',|:|') . &Hide_parens('(|)|') . &Hide_quest('?');
my $symbol = qr/[\[\]@\#&!~\`\';:,=\\\%\+\*\$\^\.\/\?\-\{\}\x22]|(?:$hidden_chars)|&\w+;/;
my $emoticon = "(?::\\-[\\(\\)])";
my $symbols_in_context = "(?:$wf_start$symbol+$wf_end(?:$wf_start(?:or)$wf_end)$wf_start$symbol+$wf_end)";

# Multi-word stoplist items
my %multi_stopwords = ();
foreach my $n (2..max_stopforms) {
    my @msw = multi_stopwords($n);
    my $re;
    foreach my $stop (@msw) {
        $stop =~ s/ /$wf_end$wf_start/go;
        $re = defined $re ? join("|", $re, $wf_start . $stop . $wf_end) : $wf_start . $stop . $wf_end;
    }
    $multi_stopwords{$n} = qr/$re/;
}

# Multi-word cardinals
my $multi_cardinals_re;
my @msw = &all_cardinals("multi");
for (@msw) {
    s/ /$wf_end$wf_start/go;
    $multi_cardinals_re = defined $multi_cardinals_re ? join("|", $multi_cardinals_re, $wf_start . $_ . $wf_end) : $wf_start . $_ . $wf_end;
}
$multi_cardinals_re = qr/$multi_cardinals_re/;

# Mathematical formulas 
my $variable = qr/[a-z]{1,2}/i;
my $variable_in_paren = qr/$wf_start[a-z]$wf_end/i;
my $operator = "[=+]";
my $trig_fn = qr/sin|cos|tan|log/;
# [FfGg](x); A(f+g)
my $function = qr/$variable\($variable(?:$operator(?:$variable|\d))?\)/;
my $delta = qr/d(?:$function|$variable)\/d$variable/;
# AB+CD=AD+CB; N-1; i=j; [Ff](x)=y; [Gg](y)=x; [Ff](x); d[Ff](x)/dx; a+bi
my $simplex_formula = qr/(?:$variable|$function)(?:$operator$variable)+|${variable}[\-^]\d+|$variable\/$variable|$delta|$function/;
# I = E/R; y = ax + b; y = kx; A(f+g) = Af + Ag; [Ff](x) + C;
# d[Ff](x)/dx = [Ff](x); a x b = b x a
my $complex_variable = qr/$wf_start(?:$variable|$function|$delta)$wf_end$wf_start(?:[+x])$wf_end$wf_start(?:$variable|$function|$delta)$wf_end/;
my $complex_variable_2 = qr/$wf_start(?:$variable|$function|$delta)$wf_end$wf_start\+$wf_end$wf_start(?:$variable|$function|$delta)$wf_end/;
my $complex_formula = qr/(?:$complex_variable|$wf_start(?:$function|$delta|$variable)$wf_end)$wf_start=$wf_end(?:$complex_variable|$wf_start(?:$simplex_formula|$variable)$wf_end)/;
# log ### = #.###; y=sin x
my $trig_formula = qr/(?:$wf_start$trig_fn$wf_end$wf_start$num$wf_end$wf_start$operator$wf_end$wf_start$num$wf_end)|$wf_start$variable(?:$wf_end$wf_start)?$operator(?:$wf_end$wf_start)?$trig_fn$wf_end$wf_start$variable$wf_end/;

# Analogies
my $is_to = qr/$wf_start[^<>]+$wf_end$wf_start[:]$wf_end(?:$wf_start\($wf_end[^()]+?$wf_start\)$wf_end|$wf_start[^<>]+$wf_end)/;
my $analogy = qr/$is_to$wf_start(?:::)$wf_end$is_to/;

# Recursive regex for (potentially nested) parentheticals.
my $parend;
$parend = qr/\((?:(?>[^\(\)]+)|(??{$parend}))*\)/;

my $lparen = qr|<wf[^>]*?type="punc"[^>]*>\(</wf>|;
my $rparen = qr|<wf[^>]*?type="punc"[^>]*>\)[ :;,!\.\?\x22]*?</wf>|;

# Semantically "empty" forms not on stoplist:
# "[,;] (used)? especially/chiefly/mainly/primarily ...",
# "[,;] usually/often/especially/normally/primarily/frequently/widely
# used|for..."

my $clause_boundary = qr/$wf_start(?:[:;,\)])$wf_end|<\/?aux[^>]*?>/;
my $used = qr/$wf_start(?:used)$wf_end/;
my $especially = qr/$wf_start(?:espe?cially|chiefly|mainly|primarily)$wf_end/;
my $usually = qr/$wf_start(?:usually|often|espe?cially|normally|primarily|frequently|widely)$wf_end/;

my $semantically_empty = qr/(?:$used(?:$wf_start(?:of)$wf_end)?)?$especially|(?:$usually)(?:$used|$wf_start(?:for)$wf_end)|$wf_start(?:of)$wf_end$especially?/;
my $semantically_empty_in_context = qr/(?:$semantically_empty)[^()]+?(?=$|$wf_start(?:[\(\):;,])$wf_end|<\/?(?:def|classif|aux|ex|qf))/;

# To be ignored within verb args: especially, like, such (when followed by as)
my $arg_ignores = qr/(?:$wf_start(?:especially|like)$wf_end|$wf_start(?:such)$wf_end(?=$wf_start(?:as)$wf_end))/;

sub find_and_hideIgnorablePunc {
    my $str = shift;

    # Hide parens in chemical formulas of the CnH(2n+2) kind
    $str =~ s/$chem_formula/&Hide_parens($&)/ego;
    # Hide : in emoticons :-) and :-(
    $str =~ s/$emoticon/&Hide_comma($&)/ego;
    
    # Hide parentheses in defs of blended forms, acronyms, etc.
    # i.e., P(ropeller) T(orpedo)...
    $str =~ s/(?:(?<!^)[^\(\ ]\([^\)\(]+?\))|(?:\([^\)\(]+?\)(?!$|[\)\ :;,!\.\?\x22]))/&Hide_parens($&)/ego;
    
    # Check for other embedded parens that should be hidden, right
    # now just parenthesized chemical symbols
    $str =~ s/($parend)/_check_for_hidableParenthetical($1)/ego;
    
    # Hide :'s in free-text chemical symbols 
    $str =~ s/$chem_free/&Hide_comma($&)/ego;
    
    # Hide any ?'s that are part of dates or are used as symbols
    # rather than punctuation.
    $str =~ s/$yr_range/&Hide_quest($&)/ego;
    $str =~ s/(?=^| )(?:$yyyy|$nyear)\??\-?(?=$| )/&Hide_quest($&)/ego;
    $str =~ s/(?=^| )$yr_range(?=$| )/&Hide_quest($&)/ego;
#    $str =~ s/\((?:\`?\?\'?|$symbol+\ +or\ +$symbol+)\)/&Hide_quest($&)/ego;
    $str =~ s/\((?:$symbol+(?:\ +or\ +$symbol+)?)\)/&Hide_quest($&)/ego;

    # Hide "," and ":" in numbers
    $str =~ s/\d[,:]\d/&Hide_comma($&)/geo;
    
    return $str;
}

sub _check_for_hidableParenthetical {
    my $parenth = shift;
    $parenth =~ s/^\(($chem_in_paren)\)$/"(" . &Hide_comma(&Hide_parens($1)) . ")"/oe;
    return $parenth;
}

sub unhideIgnorablePunc {
    my $str = shift;
    return &Unhide_parens(&Unhide_quest(&Unhide_comma($str)));
}

sub findParentheticals {
    my $tmp = shift;

    my $ignored;
    # Locate parenthesized stretches of text and pass to _ignoreParenthetical
PLOOP: {
        # ex's and classif's are not being tagged, so don't bother
        # checking in them; disallow nested aux...
        $tmp =~ s/^<(ex|classif|aux).+?<\/\1>\s*//o && do {$ignored .= $&; redo PLOOP;};
        $tmp =~ s/^<\/?(def|qf)[^>]*>\s*//o && do {$ignored .= $&; redo PLOOP;};
        $tmp =~ s/^$lparen(.+?)$rparen//o && do {$ignored .= _ignoreParenthetical($&); redo PLOOP;};
        $tmp =~ s/^$lparen(.+?)(?=$lparen)//o && do {$ignored .= $&; redo PLOOP;};
        $tmp =~ s/^$rparen(.+?)$rparen//o && do {$ignored .= $&; redo PLOOP;};
        $tmp =~ s/^<wf.*?<\/wf>\s*//o && do {$ignored .= $&; redo PLOOP;};
        $tmp =~ s/^<\/\w+>\s*//o && do {$ignored .= $&; redo PLOOP;};
    }
    print STDERR "LEFT UNTESTED [findParentheticals]:\n$tmp\n\n" if $tmp ne "";
    
    return $ignored;
}

sub _ignoreParenthetical {
    my $tmp = shift;

    my $ret;
    if ( $tmp =~ /^($lparen)($wf_start$chem_in_paren$wf_end)($rparen)$/o ) {
        $ret = $1 . _ignore_Wf("chem", $2) . $3;
    } elsif ( $tmp =~ /^($lparen)($variable_in_paren)($rparen)$/o ) {
        $ret = $1 . _ignore_Wf("math", $2) . $3;
    } elsif ( $tmp =~ /^($lparen)($wf_start$symbol+$wf_end)($rparen)$/o ) {
        $ret = $1 . _ignore_Wf("symb", $2) . $3;
    } elsif ( $tmp =~ /^($lparen)($date_range_in_context|$date_range|$month_range)($rparen)$/o ) {
        $ret = $1 . _tag_and_ignore_Mwf($2, "drange") . $3;
    } elsif ( $tmp =~ /^($lparen)($dates_in_context)($rparen)$/o ) {
        $ret = $1 . _tag_and_ignore_Mwf($2, "date") . $3;
    } elsif ( $tmp =~ /^($lparen)($symbols_in_context)($rparen)$/o ) {
        $ret = $1 . _tag_and_ignore_Mwf($2, "other") . $3;
    } elsif ( $tmp =~ /^($lparen)($percent_range|$nrange_in_context|$numeric_range)($rparen)$/o ) {
        $ret = $1 . _tag_and_ignore_Mwf($2, "nrange") . $3;
    } elsif ( $tmp =~ /^($lparen)($num_in_context|$nplusfrac)($rparen)$/o ) {
        $ret = $1 . _tag_and_ignore_Mwf($2, "num") . $3;
    } elsif ( $tmp =~ /^($lparen)($measure_range|$measure_in_context|$measure|$number$wf_start(?:in|day|month|year)s?$wf_end)($rparen)$/o ) {
        $ret = $1 . _tag_and_ignore_Mwf($2, "meas") . $3;
    } elsif ( $tmp =~ /^($lparen)($million|$currency_in_context)($rparen)$/o ) {
        $ret = $1 . _tag_and_ignore_Mwf($2, "curr") . $3;
    } elsif ( $tmp =~ /^($lparen)($time_in_context|$time)($rparen)$/o ) {
        $ret = $1 . _tag_and_ignore_Mwf($2, "time") . $3;
    } elsif ( $tmp =~ /^($lparen)($wf_start$currency$wf_end)($rparen)$/o ) {
        $ret = $1 . _ignore_Wf("curr", $2) . $3;
    } elsif ( $tmp =~ /^($lparen)($yyyy_wf)($rparen)$/o ) {
        $ret = $1 . _ignore_Wf("year", $2) . $3;
    } elsif ( $tmp =~ /^($lparen)($number)($rparen)$/io ) {
        $ret = $1 . _ignore_Wf("num", $2) . $3;
    } elsif ( $tmp =~ /^($lparen)($wf_start$oclock$wf_end)($rparen)$/o ) {
        $ret = $1 . _ignore_Wf("time", $2) . $3;
    } elsif ( $tmp =~ /^($lparen)((?:$wf_start(?:in)$wf_end(?:$yyyy_wf))|$date)($rparen)$/o ) {
        $ret = $1 . _tag_and_ignore_Mwf($2, "date") . $3;
    } elsif ( $tmp =~ /^($lparen)($semantically_empty)([^()]+?$rparen)$/o ) {
        # Just set tag="ignore" on the empty words, don't aux-tag
        my ($l, $m, $r) = ($1, $2, $3);
        $m =~ s/^$semantically_empty/_ignore_Empty($&)/geo;
        $ret = $l . $m . $r;
        return $ret;
    } else {
        return findIgnores($tmp);
    }
    return &tag_elt($ret, "aux", {tag => "ignore"});
}

sub findIgnores {
    my $str = shift;
    
    my $ignored;
    # Test spans of wfs for ignorableness. Treat <aux>, <qf>, <ex>
    # and their end tags as boundaries over which text cannot span.
    # Skip already-tagged mwf's and (ignorable) aux's & punctuation.
ILOOP:  {
        $str =~ s/^\ +?(?=$|[^\s])//o && do {$ignored .= $&; redo ILOOP;};
        $str =~ s/^<mwf[^>]*?>.+?<\/mwf>\ *//o && do {$ignored .= $&; redo ILOOP;};
        # Ignore Christiane's list for ignorables w/in <aux tag="arg">.
        $str =~ s/^(<aux[^>]*?tag="arg"[^>]*>)(.+?)(<\/aux>\ *)//o && do {$ignored .= $1 . _argIgnore($2) . $3; redo ILOOP;};
        # Only skip "ignore" aux's
        $str =~ s/^<aux[^>]*?tag="ignore"[^>]*>.+?<\/aux>\ *//o && do {$ignored .= $&; redo ILOOP;};
        # Ignore semantically "empty" wfs
        $str =~ s/^($clause_boundary)($semantically_empty_in_context)//o && do {
            my ($l, $r) = ($1, $2);
            $r =~ s/^($semantically_empty)(.*?)$/_ignore_Empty($1)/geo;
            $str = $2 . $str;
            $ignored .= $l . $r;
            redo ILOOP;
        };
        # Do not skip "ignore" wf's because these may have been set by the
        # tokenizer. Let them pass to _ignoreStr() and be handled below. 
        # However, DO skip type="punc" wf's!
        $str =~ s/^<wf[^>]*?type="punc"[^>]*>.+?<\/wf>\ *//o && do {$ignored .= $&; redo ILOOP;};
        $str =~ s/^<\/?(?:def|classif|aux|ex|qf)[^>]*>//o && do {$ignored .= $&; redo ILOOP;};
        $str =~ s/^.+?(?=$|<\/?(?:def|classif|aux|ex|qf))//o && do {
            my $try_to_ignore = _ignoreStr($&);
            $try_to_ignore =~ s/^\s*?<(m?wf).+?<\/\1>\s*?(?=$|[^\s])//o;
            $ignored .= $&;
            $str = $try_to_ignore . $str;
            redo ILOOP;
        };
    }
    print STDERR "LEFT UNTESTED [findIgnores]:\n$str\n\n" if $str ne "";
    
    return $ignored;
}

sub _ignore_Empty {
    my $tmp = shift;
    $tmp =~ s/(?:$especially|$usually)/_ignore_Wf("empty", $&)/geo;
    return $tmp;
}

sub _tag_and_ignore_Mwf {
    my ($mwf, $mwf_type) = @_;

    if ( $mwf_type eq "stop" ) {
        # Is stoplist item. Set tag=ignore on all wfs, but don't mwf-wrap
        $mwf =~ s/<wf[^>]*>.+?<\/wf>/_ignore_Wf($mwf_type, $&)/geo;
        return $mwf;
    } elsif ( $mwf_type eq "cardinal" ) {
        # Is multi-word cardinal. Set tag=ignore on all wfs, but don't mwf-wrap
        $mwf =~ s/<wf[^>]*>.+?<\/wf>/_determine_wf_Type($&, $mwf_type)/geo;    
        return $mwf;
    } else {
        $mwf =~ s/<wf[^>]*>.+?<\/wf>/_determine_wf_Type($&, $mwf_type)/geo;    
        return &tag_elt($mwf, "mwf", {type => "$mwf_type"});
    }
}

sub _determine_wf_Type {
    my ($elt, $parent_type) = @_;
    
    {
        $elt =~ m/^\s*(?:$wf_start$chem_formula$wf_end)$/o && do {$elt = _ignore_Wf("chem", $elt); last;};
        $elt =~ m/^\s*(?:$wf_start$chem_free$wf_end)$/o && do {$elt = _ignore_Wf("chem", $elt); last;};
        $elt =~ m/^\s*(?:$wf_start$simplex_formula$wf_end)$/o && do {$elt = _ignore_Wf("math", $elt); last;};
        $elt =~ m/^\s*(?:$wf_start$oclock$wf_end)$/o && do {$elt = _ignore_Wf("time", $elt); last;};
        $elt =~ m/^\s*(?:$wf_start$currency$wf_end)$/o && do {$elt = _ignore_Wf("curr", $elt); last;};
        $elt =~ m/^\s*(?:$yyyy_wf)$/o && $parent_type =~ /(?:date|drange)/o && do {$elt = _ignore_Wf("year", $elt); last;};
        $elt =~ m/^\s*(?:$yr_range_wf)$/o && $parent_type =~ /(?:date|drange)/o && do {
            $elt = _split_range($elt);
            $elt =~ s{<wf.+?</wf>}{_ignore_Wf("year", $&)}geo; 
            last;
        };
        $elt =~ m/^\s*$nrange$/o && $parent_type =~ /time/o && do {
            $elt = _split_range($elt);
            $elt =~ s{<wf.+?</wf>}{_ignore_Wf("time", $&)}geo; 
            last;
        };
        $elt =~ m/^\s*$nrange$/o && do {
            $elt = _split_range($elt);
            $elt =~ s{<wf.+?</wf>}{_ignore_Wf("num", $&)}geo; 
            last;
        };
        # Can't assume that any number w/in a date is a year!
#        $elt =~ m/^\s*(?:$number)$/o && $parent_type =~ /(?:date|drange)/ && do {$elt = _ignore_Wf("year", $elt); last;};
        $elt =~ m/^\s*(?:$number)$/o && $parent_type =~ /time/o && do {$elt = _ignore_Wf("time", $elt); last;};
        $elt =~ m/^\s*(?:$prange)$/o && do {
            $elt = _split_range($elt);
            $elt =~ s{<wf.+?</wf>}{_ignore_Wf("num", $&)}geo; 
            last;
        };
        # Ordinals are to be tagged as adjs, so don't ignore
#        $elt =~ m/^\s*(?:$nth)$/o && do {$elt = _ignore_Wf("num", $elt); last;};
        $elt =~ m/^\s*(?:$number)$/o && do {$elt = _ignore_Wf("num", $elt); last;};
        $elt =~ m/^\s*(?:$wf_start$symbol+$wf_end)$/o && $parent_type !~ /(?:date|drange)/ && do {$elt = _ignore_Wf("symb", $elt); last;};
        # Pick up any math variables missed
        $elt =~ m/^\s*(?:<wf.+?<\/wf>)$/o && $parent_type eq "math" && do {$elt = _ignore_Wf("math", $elt); last;};
    }
    
    return $elt;
}

sub _ignore_Wf {
    my ($wf_type, $wf) = @_;
    # noauto=T is no longer relevant on tag=ignore wfs
    $wf = &deleteAttribute("noauto", $wf);
    my %attrs = (tag => "ignore");
    if ($wf_type eq "punc") {
        $wf = &deleteAttribute("lemma", $wf);
    } elsif ( $wf_type !~ /stop|empty|arg/o ) {
        # Don't replace lemma if part of a stoplist colloc,
        # "empty" phrase, or w/in verb arg
        $wf = &assign_pseudolemma($wf);
        $attrs{type} = $wf_type;
    }
    return &setAttributeValues($wf, \%attrs);
}

# Ignorable text within verb args
sub _argIgnore {
    my $istr = shift;
    $istr =~ s/$arg_ignores/_ignore_Wf("arg", $&)/geo;
    return $istr;
}

sub _ignoreStr {
    my $istr = shift;
    
    my $ig_end = qr/(?=$|\s*<\w)/;

    # Tests the START of string for all ignores, longest to shortest. 
    # If found, the calling routine will handle skipping it on the next go around.
    # If not found, calling routine will skip over the first <wf>...</wf>, and
    # start the process over again.
ISTR: {
        last if $istr =~ s/^($measure_range)$ig_end/_tag_and_ignore_Mwf($1,'meas')/oe;
        last if $istr =~ s/^($measure_per)$ig_end/_tag_and_ignore_Mwf($1,'meas')/oe;
        last if $istr =~ s/^($measure)$ig_end/_tag_and_ignore_Mwf($1,'meas')/oe;
        last if $istr =~ s/^($analogy)$ig_end/_tag_and_ignore_Mwf($1,'other')/oe;
        last if $istr =~ s/^($complex_formula|$trig_formula|$complex_variable_2)$ig_end/_tag_and_ignore_Mwf($1,'math')/oe;
        last if $istr =~ s/^($million)$ig_end/_tag_and_ignore_Mwf($1,'curr')/oe;
        last if $istr =~ s/^($nplusfrac)$ig_end/_tag_and_ignore_Mwf($1,'num')/oe;
        last if $istr =~ s/^($time)$ig_end/_tag_and_ignore_Mwf($1,'time')/oe;
        last if $istr =~ s/^($percent_range|$prange)$ig_end/_tag_and_ignore_Mwf($1,'nrange')/oe;
        last if $istr =~ s/^($date_range|$yr_range_wf)$ig_end/_tag_and_ignore_Mwf($1,'drange')/oe;
        last if $istr =~ s/^($month_range)$ig_end/_tag_and_ignore_Mwf($1,'drange')/oe;
        last if $istr =~ s/^($ad_date)$ig_end/_tag_and_ignore_Mwf($1,'date')/oe;
        last if $istr =~ s/^($bc_date)$ig_end/_tag_and_ignore_Mwf($1,'date')/oe;
        last if $istr =~ s/^($century)$ig_end/_tag_and_ignore_Mwf($1,'date')/oe;
        last if $istr =~ s/^($modayyr)$ig_end/_tag_and_ignore_Mwf($1,'date')/oe;
        last if $istr =~ s/^($daymoyr)$ig_end/_tag_and_ignore_Mwf($1,'date')/oe;
        last if $istr =~ s/^($wf_start(?:in)$wf_end)($yyyy_wf)$ig_end/$1 . _determine_wf_Type($2,'date')/oe;
        last if $istr =~ s/^($numeric_range|$nrange)$ig_end/_tag_and_ignore_Mwf($1,'nrange')/oe;
        last if $istr =~ s/^($wf_start$emoticon$wf_end)$ig_end/_ignore_Wf('symb', $1)/oe;
        foreach my $n (2..max_stopforms) {
            last ISTR if $istr =~ s/^($multi_stopwords{$n})$ig_end/_tag_and_ignore_Mwf($1,'stop')/e;
        }
        last if $istr =~ s/^($multi_cardinals_re)$ig_end/_tag_and_ignore_Mwf($1,'cardinal')/oe;
        last if $istr =~ s/^(<wf.+?<\/wf>)/_determine_wf_Type($1,"")/oe;
    }
    return $istr;
}

sub _split_range {
    my $elt = shift;
    $elt = &deleteAttribute("sep", $elt);
    $elt =~ s{(<wf[^>]*)(>[^=\-]+?)([=\-])([^<]+?)(</wf>)}{"$1 sep=\"$3\"$2$5" . &tag_elt($4, "wf")}oe;
    return $elt;
}

1;

=head1 NAME

Tag::Ignore -- Recognize and tag "ignorable" text in wn glosses.

=head1 DESCRIPTION

Ignorable text consists of numeric info such as dates, date ranges,
measurements, chemical symbols, etc. Ignorable text can be individual
items (e.g., 1492), collocations (e.g., 100 ft.), or parenthesized
stretches of text containing other, non-ignorable, text (e.g.,
(approximately 20 miles)). Multi-form stoplist items is another class
of ignorables.

=head1 Functions

=over 4

=item B<find_and_hideIgnorablePunc(>I<untagged_text>B<)>

Takes as input the (untagged) contents of a def and searches for
punctuation that is part of a word or number and hides it. 

=item B<findParentheticals(>I<tokenized_text>B<)>

Takes as input the tokenized contents of a def and searches for
parenthesized stretches of text. Each parenthetical is passed to
_ignoreParenthetical(), which does the actual testing for ignorable text.

=item B<_ignoreParenthetical(>I<tokenized_text>B<)>

Takes as input a parenthesized stretch of tokenized text from a gloss and 
tests for ignorable info, within certain contexts, as its sole contents. If
found, tag="ignore" is assigned to ignorable wfs, and the entire
parenthesized stretch is <aux>-tagged.

Note that the <aux> itself is not set to tag="ignore", since not all of
the constituent wfs are ignorable.

=item B<findIgnores(>I<tokenized_search_string>B<)>

Searches (tokenized) input string for instances of free (unparenthesized) ignorable text such as dates, date ranges, measurements, numbers, etc.

=item B<_tag_and_ignore_Mwf(>I<mwf>, I<mwf_type>B<)>

Wraps input multi-word ignorable text in <mwf> tags and assigns type=mwf_type. Each <wf> is passed to _determine_wf_Type() and tested for ignore status. The mwf itself is _not_ assigned tag="ignore", as all or part of it may be tagged at a later stage.

Note that the subordinate <wf>s of an mwf are not truly lemmatized. Instead, they are assigned a pseudo-lemma form that is simply the <wf>'s contents. The mwf itself is not assigned a lemma.

=item B<_determine_wf_Type(>I<wf>, I<parent_type>B<)>

Tests input <wf> against criteria for one of the ignorable categories
of text. The type= is assigned based on the contents of the <wf> and parent_type
(the value of the type= attribute of the parent mwf).

Side effect: will split wfs whose contents are a date or numeric range
(e.g., 1998-2003) into two wfs.

=item B<_ignore_Wf(>I<wf_type>, I<wf>B<)>

Assign tag="ignore" and type=wf_type to input wf.

=item B<_ignoreStr(>I<tokenized_search_string>B<)>

Tests (tokenized) input text for all free ignorable text strings. Multi-word
examples are passed to _tag_and_ignore_Mwf(), which assigns a type to
the entire <mwf> and handles testing the individual <wf>s for
ignorableness. If all tests for multi-word ignores fail, the first
<wf> is passed to _determine_wf_Type() for testing.

    
