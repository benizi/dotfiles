package Tag::Tokenizer;

# TODO: use unicode encoding for <, >, and &?

use 5.006;
use strict;
use warnings;

use Tag::Ignore qw/find_and_hideIgnorablePunc unhideIgnorablePunc/;

require Tag::Gloss;
require Tag::Stoplist;
require Tag::Apostrophe;
require Tag::Functions;

sub import {
	my @EXPORT = qw(tokenize parse_and_tokenize_Gloss);
	my $package = shift;
	my ($caller, $file, $line) = caller();

	Tag::Stoplist->import(@_);
	Tag::Apostrophe->import(@_);
	Tag::Gloss->import(@_);

	Tag::Functions->import(@_);
	
	no strict 'refs';
	foreach my $export (@EXPORT) {
		*{"$caller\::$export"} = *{"$package\::$export"};
	}
}

my $delims = "(?:\\ |::|\\-\\-|\\.\\.\\.\\.|\\.\\.\\.|\\.\\.|[:;,!\\(\\)\\?\\x22])";
my $fpunc = "[:;,!\\(\\)\\.\\?]";
my $fpunc_exparens = "[:;,!\\.\\?]";


sub parse_and_tokenize_Gloss {
	my ($str, $tokenize_exs, $pos) = @_;
	$tokenize_exs ||= 0;
	$pos ||= "";
	return undef if !defined($str) || $str eq "";

	# Just in case pre-parsed data is passed in...
	$str = Gloss_safety_measure($str);

	if ( $str !~ /<(?:def|ex|aux)/io ) {
		# Hide <, >, and & if unparsed data is passed in
		$str = Gloss_prep($str);
		# Parse the gloss
		$str = parseGl($str, $pos);
	}

	my $tokenized;
	{
		# Skip already-tokenized text (this should be unnecessary, unless
		# routine called with already-tokenized text for some reason)
		$str =~ s/^<(m?wf)[^<>]*>.+?<\/\1>//o && do {$tokenized .= $&; redo;};
		# If tokenizing examples, call tokenizer with $is_sentence flag = 1
		$tokenize_exs && $str =~ s/^(<ex[^<>]*>)(.+?)(?=<\/ex>)//o && do {$tokenized .= $1 . tokenize($2, 1); redo;};
		# If not tokenizing examples, skip entire example
		!$tokenize_exs && $str =~ s/^<ex[^<>]*>.+?<\/ex>//o && do {$tokenized .= $&; redo;};
		# Skip over other start and end tags
		$str =~ s/^<\/?[^\s<>]+?[^<>]*>\s*//o && do {$tokenized .= $&; redo;};
		# Between-tag contents (includes <def> and gloss-level <aux>).
		# Call tokenizer on untagged stretches of text
		$str =~ s/^[^<>]+?(?=$|<)//o && do {$tokenized .= tokenize($&); redo;};
	}
	print STDERR "LEFT UNTOKENIZED [parse_and_tokenize_Gloss]:\n$str\n\n" if $str ne "";

	# Suck out spaces
	$tokenized =~ s/>\ +(?=$|[^\ ])/>/go;

	# Convert <, >, and & to entities
	$tokenized = Gloss_post($tokenized);

	return $tokenized;
}

sub tokenize {
	my ($str, $is_sentence) = @_;
	$is_sentence ||= 0;

	$str = find_and_hideIgnorablePunc($str);
	$str = find_and_hideApostrophes($str);

	# Tag double-quoted stretches
	$str =~ s/\"([^\"]+?)\"/ <dq> $1 <\/dq> /go;

	# Tag single-quoted stretches as <sq>...</sq>, then restore previously
	# hidden apostrophes. Must be done prior to tokenization, as
	# will be assigning lemma and want apostrophes intact and
	# quotemarks out of the picture
	# NOTE: Inserting spaces around the tags is a major cheat so that
	# the tokenizer will treat them as "words". Will suck them out later...
	$str =~ s/\`([^\']+?)\'/ <sq> $1 <\/sq> /go;
	
	$str = restore_Apostrophes($str);

	my @words = split (/($delims)/, $str);

	$str = "";
	my $prevword = "";
	my $last_index = 0;
	for ($last_index = $#words; $last_index >= 0; $last_index--) {
		next if $words[$last_index] =~ /^\ *$/o;
		last if $words[$last_index] !~ /^(?:<[^>]+?>|$fpunc+)$/o;
	}

	my $index = -1;
	while (1) {
		$index++;
		my $word = shift @words;
		last if !defined $word;
		next if $word =~ /^ *$/o;
		my %attrs = ();
		# Lookaheads for next 2 tokens (including spaces)...
		my @lookahead;
		my $n = 0;
		my $nextword = "";
		for (@words) {
			if ($_ ne "") {
				$lookahead[$n] = $_;
				# Save next word for call to is_abbreviation()
				$nextword = $_ if $nextword =~ /^\ *$/o;
				last if $n++ == 1;
			}
		}
		# Set sep="" on wfs not followed by a space, on open parens
		# followed by quotes, and on the last wf in a qf
		if ( defined $lookahead[0] &&
			 (($lookahead[0] ne " ") ||
			  (($lookahead[1] ||= '') =~ /<\/[ds]q>/o) ||
			  ($word eq "(" && ($lookahead[0] =~ /<[ds]q>/o || ($lookahead[0] eq " " && ($lookahead[1] || '') =~ /<[ds]q>/o)))) ) {
			$attrs{sep} = "";
		}
		if ( $word =~ m/(?:<[^<>]+?>|\ )/o ) {
			$str .= $word;
		} elsif ( $word =~ m/$delims/o || $word =~ /^[\.\-]$/o ) {
			$str .= tag_elt_as_punc($word, \%attrs);
		} else {
			my $wf;

			# Restore commas & colons and tag
			$word = unhideIgnorablePunc($word);

			if ( my @pairs = decontract($word) ) {
				# Word is a contraction, so split it. For each part, set rdf=
				# if the returned lemma differs from the orthographic form.
				while (@pairs) {
					my $lemma = shift @pairs;
					my $orth = shift @pairs;
					if (!@pairs) {
						$attrs{rdf} = $lemma if $orth ne $lemma;
						$wf = tag_elt($orth, "wf", \%attrs);
						last;
					} else {
						my %cattrs;
						$cattrs{rdf} = $lemma if $orth ne $lemma;
						$cattrs{sep} = "";
						$str .= tag_elt($orth, "wf", \%cattrs);
					}
				}
			} else {
				# Recognize (but don't sense-tag) abbrevs/acronyms
				if ( my ($lemma, $rdf, $type, $lemma_sk_pairs) = is_abbreviation($word, $prevword, $nextword) ) {
					if ( !defined $lemma_sk_pairs) {
						$attrs{rdf} = $rdf;
						$attrs{type} = $type;
						$attrs{lemma} = $lemma;
						$attrs{tag} = "ignore" if !is_noauto($word);
					} elsif ( scalar @$lemma_sk_pairs > 0 ) {
						while (@$lemma_sk_pairs) {
							my $lemma = shift @$lemma_sk_pairs;
							my $sk = shift @$lemma_sk_pairs;
							$sk =~ /%\d+(?=:)/o;
							my $pos = $&;
							if ( !defined $attrs{lemma} ) {
								$attrs{lemma} = "$lemma$pos";
							} elsif ( $attrs{lemma} !~ /$pos/ ) {
								$attrs{lemma} = join("|", $attrs{lemma}, "$lemma$pos");
							}
						}
						$attrs{rdf} = $rdf;
						$attrs{type} = $type;
						$attrs{tag} = "un";
					}
				}
				$wf = tag_elt($word, "wf", \%attrs);
			}

			# Move sentence/clause-final punctuation other than parens outside end
			# </wf> tag, after first checking that:
			#	  (1) it is not part of the lemma
			#	  (2) it is not being used as a symbol (or as part of one), 
			#		  but is in fact a punctuation mark
			#	  (3) it's not a question mark that is part of a year, e.g.,
			#		  518?-318?
			#	  (4) if it's a period, then it's found at the end of an
			#		  example sentence.
			# Parens are not moved out since if a wf has parens in it, then
			# they are part of the wf, e.g., chemical symbols such as
			# Pb(CH3CO)2. Punctuation that is moved out is wf-tagged.
			my $lemma = (getAttributeValue('lemma', $wf) || '');
			my $type = (getAttributeValue('type', $wf) || '');
			unless ( ($lemma =~ /(?:$fpunc_exparens)$/o) || 
					 ($word =~ /^$fpunc_exparens+$/o) ||
					 ($word =~ /\-\d+\?$/o) ||
					 ($wf =~ /\.<\//o && (($type =~ /abbr|acronym/o) || $word =~/^(?:[A-Z]\.)+$/o || ($is_sentence && ($index != $last_index)))) ) {
				if ($wf =~ s/($fpunc_exparens)(?=<\/[^<>]+?>)//o) {
					my $punc = $1;
					my %attrs = ();
					my $sep = getAttributeValue('sep', $wf);
					$attrs{sep} = $sep if defined $sep;
					# Replace sep= on wf with "", old value moves to newly-created wf.
					$wf = setAttributeValues($wf, {sep => ""}) . tag_elt_as_punc($punc, \%attrs);
				}
			}
			$str .= $wf;
			$prevword = $word;
		}
	}
	# Suck out those spaces
	$str =~ s/\s(<\/?[sd]q[^>]*>)\s/$1/go;
	$str =~ s/(\s+?)(<\/mwf>)/$2$1/go;

	# Move any final punc outside </mwf>
	$str =~ s/(<wf[^>]*?type="punc"[^>]*>$fpunc<\/wf>)\ *(<\/mwf>)/$2$1/go;
	
	# Do (remaining) stoplist items, then lemmatize
	my $old_str = $str;
	$str = "";
	$index = 0;
	{
		$old_str =~ s/^[^<>]+?(?=$|<)//o && do {$str .= $&; redo;};
		# First check for single-word stoplist items (assumes that
		# trailing punctuation has been moved out). Then assign a lemma 
		# to any wf left without one.
		$old_str =~ s/^<wf[^<>]*>(.+?)<\/wf>//o && do {
			my $wf = $&;
			my $contents = getAttributeValue('rdf', $wf);
			$contents ||= $1;
			my $special_handling = ($is_sentence && (++$index == 1)) ? 1 : 0;
			unless ( hasAttributeValue('type', "", $wf) ) {
				if ( my $stop = is_stoplist($contents, $special_handling) ) {
					$wf = setAttributeValues($wf, {lemma => $stop, tag => "ignore"});
				} else {
					$wf = assign_potential_lemmas($wf, $special_handling);
					my %attrs = (tag => "un");
					#$contents =~ s/'s$//o;
					$wf = setAttributeValues($wf, \%attrs);
				}
			}
			$str .= $wf;
			redo;
		};
		$old_str =~ s/^<\/?(?:classif|mwf|aux|ex|sq|dq)[^<>]*>//o && do {$str .= $&; redo;};
	}
	print STDERR "LEFT UNCHECKED [tokenize]:\n$old_str\n\n" if $old_str ne "";
	
	# Tag quotes (doing it this way is a very big cheat to avoid
	# recursively handling embedded quotes)
	$str =~ s/<([ds]q)>/<qf rend="$1">/go;
	$str =~ s/<\/[ds]q>/<\/qf>/go;

	return $str;
}

1;

=head1 NAME

Tag::Tokenizer

=head1 DESCRIPTION

Parses gloss and tokenizes into first pass of wfs (prior to globbing
stage). Stop & waitlist words are handled here, along with splitting
of contractions. Abbreviations and acronyms are recognized, though not
sense-tagged. Single- and double-quoted text is wrapped in <qf> tags.
Punctuation is wf-tagged.

=head1 Functions

=over 4

=item B<parse_and_tokenize_Gloss(>I<untokenized_gloss>, I<tokenize_egs_flag>B<)>

Takes input gloss and parses it into <aux>, <def>, and <ex>, then
tokenizes the contents of (gloss-level) <aux> and <def>. Example
sentences (<ex>) are tokenized only if $tokenize_egs_flag is non-zero.

=item B<tokenize(>I<text_string>, <$is_sentence>B<)>

Tokenizes input (untagged) text string. Handles single-word stoplist
items, waitlist and other "noauto" words (in get_lemmas_by_moaning),
punctuation, and single-quoted text. 

If $is_sentence is non-zero, then the text is an example sentence, in
which case possible capitalization of the first word and period as final
punctuation must be dealt with.

