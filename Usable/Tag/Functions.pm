package Tag::Functions;

use 5.006;
use strict;
use warnings;

use WordNet;
use WordNet::Morphology;

require Tag::Apostrophe;
require Tag::Classif;
require Tag::Stoplist;

my @defaults = ( inf_loc  => '/wordnet/wn/2k3/lib/perl/various',
				 px_file => 'prefix.ix',
				 fw_file => 'fn_words.ix',
				 hm_file => 'hyp_misc.ix',
				 vfile => 'abbrevs.tok',
				 rh_file => 'reverse_hyphens.ix',
				 oc_file => 'ord_card.tok',
				 af_file => 'alloforms.ix',
				 sp_file => 'special.tok',
				 morphdir  => '/wordnet/wn/2k3/lib/perl/various',
				 split_nonwords => 0,
				 case_sensitive => 1
			   );

my ($px_file, $fw_file, $hm_file, $abbr_file, $rh_file, $oc_file, $af_file, $sp_file);
my $abbr_initialized = my $ix_initialized = my $rh_initialized = my $oc_initialized = my $sp_initialized = 0;
my $case_sensitive = 1;
my $split_nonwords = 0;

my ($wn, $morph);

sub import {
	my @EXPORT = qw(XML_prep XML_post Gloss_prep Gloss_post Gloss_safety_measure 
	   Hide_apos Unhide_apos Hide_comma Unhide_comma
	   Hide_parens Unhide_parens Hide_quest Unhide_quest
	   hasAttributeValue getAttributeValue setAttributeValues setContents
	   deleteAttribute assign_potential_lemmas assign_pseudolemma zpad
	   sense_tag_tf build_mwf_lemma tag_elt tag_elt_as_punc format_XML_contents_tf
	   timestamp combinations concatenations
	   wrap_synset_elts get_lemmas_by_moaning decontract 
	   get_all_forms assign_ids_to_wfs next_id_tf index_info_formatted_for_sql 
	   is_abbreviation initialize_preprocessor is_cardinal all_cardinals);
	
	my $package = shift;
	my ($caller, $file, $line) = caller();

	use File::Spec;

	push @defaults, $_ for @_;

	my %param = (@defaults);
	$px_file = File::Spec->catfile($param{inf_loc}, $param{px_file});
	$fw_file = File::Spec->catfile($param{inf_loc}, $param{fw_file});
	$hm_file = File::Spec->catfile($param{inf_loc}, $param{hm_file});
	$rh_file = File::Spec->catfile($param{inf_loc}, $param{rh_file});
	$oc_file = File::Spec->catfile($param{inf_loc}, $param{oc_file});
	$af_file = File::Spec->catfile($param{inf_loc}, $param{af_file});
	$abbr_file = File::Spec->catfile($param{inf_loc}, $param{vfile});
	$sp_file = File::Spec->catfile($param{inf_loc}, $param{sp_file});

	$case_sensitive = $param{case_sensitive};
	$split_nonwords = $param{split_nonwords};

	Tag::Apostrophe->import(@defaults);
	Tag::Classif->import(@defaults);
	Tag::Stoplist->import(@defaults);
	
	no strict 'refs';
	foreach my $export (@EXPORT) {
		*{"$caller\::$export"} = *{"$package\::$export"};
	}
	_initIx() unless $ix_initialized;
	_loadAbbrevs() unless $abbr_initialized;
	_loadOCNums() unless $oc_initialized;
	_loadReverseHyphenateds() unless $rh_initialized;
	_load_special() unless $sp_initialized;
}

my %reverse_hyphenated = ();
my %cardinal_numbers = ();
my %unmoanable_nonwords = ();

sub timestamp {
	my ($t) = @_;
	$t ||= time;
	my (@a) = reverse +(localtime($t))[0..5];
	$a[0]+=1900;
	$a[1]++;
	sprintf '%04d'.'%02d'x5, @a;
}

sub combinations {
	# takes [ [1,2], 3, [4,5] ] to: ( [1,3,4],[1,3,5],[2,3,4],[2,3,5] )
	my $l = shift;
	my @result = map { ref eq 'ARRAY' ? (map { [$_] } @$_) : [$_]; } pop @$l;
	while (@$l) {
		my @new;
		my $c = pop @$l;
		for my $i (ref($c) eq 'ARRAY' ? (@$c) : $c) {
			push @new, map { [ $i, @$_ ] } @result;
		}
		@result = @new;
	}
	@result;
}

sub concatenations {
	# (1, 2, 3) -> ([1,2,3],[1_2,3],[1,2_3],[1_2_3])
	my @ret = [shift];
	while (@_) {
		my $e = shift;
		@ret = map { ([@$_, $e], [@{$_}[0..$#$_-1], $_->[$#$_].'_'.$e]) } @ret;
	}
	@ret;
}

sub initialize_preprocessor {
	$| = 1;
	print "Initializing preprocessor.";
	my ($apos, $apos_file) = &Tag::Apostrophe::defaults;
	my $cfile = &classif_file;
	my %classifs = ();
	my %WN_synsets_by_alloform = ();
	
	%reverse_hyphenated = ();

	$wn = WordNet->new(@defaults) unless defined $wn;
	my $it = $wn->iterator(persec=>disp=>'preprocessing');
	while ( my $ss = $it->next ) {
		foreach my $class (qw(;c ;r ;u)) {
			foreach my $ptr ($ss->pointers(qr/$class/)) {
				# Pointer target is the classif synset
				my $sk = $ptr->follow->sk;
				$sk =~ s/^[^%]+?(?=%)//o;
				# note: lemmas have no underscores (default)
				my $lemma = ($ptr->follow->words)[0];
				foreach my $lemma (($ptr->follow->words)) {
					(my $sk_lemma = lc $lemma) =~ s/ /_/go;
					$classifs{$lemma}{sk} = join("", $sk_lemma, $sk);
					$classifs{$lemma}{class} = $class;
				}
			}
		}
		my $is_oc = 0;
		foreach my $ptr ($ss->pointers('&')) {
			# Can't get sense no, so use head_wd to exclude
			# cardinal=important sense
			my $sk = $ptr->follow->sk;
			$sk =~ /^(?:[^:]*?:){3}([^:]*?):/o;
			my $head_wd = $1;
			if ( $ptr->follow->pos eq "a" && (($ptr->follow->words)[0] eq "cardinal") && (($head_wd ||= '') ne "important") ) {
				$is_oc = 1;
				last;
			}
		}
		foreach my $word ($ss->words) {
			$word =~ s/\"//go;
			# Cardinal numbers (other than those that have non-numeric senses)
			# nb. ordinals are treated as adjs
			$cardinal_numbers{$word}++ if $is_oc && $word !~ /^(?:[a-z]|c[dcl]|l[ix]|il|ane)$/io;
			# Reverse hyphenated word list (unhyphenated versions of
			# hyphenated WordNet forms)
			if ( (my $dehyph = _normalize($word)) =~ s/\-//go ) {
				(my $sp = _normalize($word)) =~ s/\-/ /go;
				$reverse_hyphenated{$dehyph}{$word}++;
				$reverse_hyphenated{$dehyph}{$sp}++;
				$reverse_hyphenated{$sp}{$word}++;
				$reverse_hyphenated{$sp}{$dehyph}++;
				if (!$case_sensitive && ($word ne lc $word)) {
					$reverse_hyphenated{lc $dehyph}{$word}++;
					$reverse_hyphenated{lc $dehyph}{$sp}++;
					$reverse_hyphenated{lc $sp}{$word}++;
					$reverse_hyphenated{lc $sp}{$dehyph}++;
				}
			}
			# Apostrophes
			(my $w = $word) =~ s/\./\\./go;
			my @cfs = split ' ', $w;			
			$$apos{scalar(@cfs)}{$w}++ if @cfs > 1 && $w =~ /.'./o;
			foreach my $cf (@cfs) {
				next if $cf !~ /'/o;
				$$apos{1}{$cf}++ if $cf =~ /.'./o && !defined $$apos{1}{$cf};
				my @dfs = split '-', $cf;
				foreach my $df (@dfs) {
					$$apos{1}{$df}++ if $df =~ /.'./o && !defined $$apos{1}{$df};
				}
			}
			# Save synset key & original WordNet form for each alloform class
			my $sk = $ss->sk;
			$sk =~ s/^[^%]+(?=%)//o;
			(my $alloform = $word) =~ s/[ _-]//g;
			$WN_synsets_by_alloform{$case_sensitive ? $alloform : lc $alloform}{sk}{$sk}++;
			$WN_synsets_by_alloform{$case_sensitive ? $alloform : lc $alloform}{lemma}{$word}++;
		}
	}
	print "\n";
	# Reverse-hyphenated terms
	my $fh = _open_autogenerated($rh_file);
	foreach my $rev (sort keys %reverse_hyphenated) {
		print $fh "$rev\t$_\n" for (keys %{$reverse_hyphenated{$rev}});
	}
	close ($fh);
	# Cardinal numbers
	$fh = _open_autogenerated($oc_file);
	print $fh "$_\n" for (sort keys %cardinal_numbers);
	close ($fh);
	# Classification terms
	$fh = _open_autogenerated($cfile);
	foreach my $lemma (sort keys %classifs) {
		print $fh "$lemma,$classifs{$lemma}{class},$classifs{$lemma}{sk}\n";
	}
	close ($fh);
	# Apostrophes
	$fh = _open_autogenerated($apos_file);
	foreach my $num (sort {$b cmp $a} keys %$apos) {
		print $fh "$_\n" for (sort {length($b) <=> length($a) || $a cmp $b} keys %{$$apos{$num}});
	}
	close ($fh);
	# Alloforms
	$fh = _open_autogenerated($af_file);
	foreach my $af (sort keys %WN_synsets_by_alloform) {
		# An alloform class is characterized by (1) there is more than one
		# member of the class and (2) they appear in more than one synset
		next unless (keys %{$WN_synsets_by_alloform{$af}{sk}} > 1) && (keys %{$WN_synsets_by_alloform{$af}{lemma}} > 1);
		my $lemmas;
		foreach my $lemma (keys %{$WN_synsets_by_alloform{$af}{lemma}}) {
			$lemmas = defined $lemmas ? join("|", $lemmas, $lemma) : $lemma;
		}
		print $fh "$af\t$lemmas\n";
	}
	close ($fh);
	$| = 0;
	$rh_initialized = $oc_initialized = 1;
}

sub dereverse_hyphenated {
	my $form = shift;
	return keys %{$reverse_hyphenated{$case_sensitive ? $form : lc $form}};
}

sub is_cardinal {
	return defined $cardinal_numbers{($_[0] ||= "")};
}

# n=single for one-word cardinals, n=multi for multi-word only, no value
# means all cardinals
sub all_cardinals {
	my $n = shift;
	return keys %cardinal_numbers unless defined $n;
	my @cardinals = ();
	foreach my $c (keys %cardinal_numbers) {
		my @cfs = split ' ', $c;
		push @cardinals, $c if (scalar (@cfs) == 1 && $n eq "single") or (scalar (@cfs) > 1 && $n eq "multi");
	}
	return @cardinals;
}

sub _open_autogenerated {
	my $fn = shift;
	require FileHandle;
	my $fh = new FileHandle;
	unlink($fn);
	open ($fh, ">$fn") or die "Couldn't open $fn for output!\n";
	print $fh "# auto-generated file $fn\n";
	$fh;
}

sub _loadReverseHyphenateds {
	%reverse_hyphenated = ();
	if (!-e $rh_file) {
		warn "$rh_file does not exist\n";
		return;
	}
	open (RH, "<$rh_file") or die "Couldn't open $rh_file!\n";
	while (<RH>) {
		s/[\n\r\s]+?$//g;
		next if /^#/ or !/\w/;
		next unless /^([^\t]+?)\t(.+?)$/;
		$reverse_hyphenated{$case_sensitive ? $1 : lc $1}{$2}++;
	}
	close (RH);
	$rh_initialized = 1;
}

sub _loadOCNums {
	%cardinal_numbers = ();
	if (!-e $oc_file) {
		warn "$oc_file does not exist\n";
		return;
	}
	open (OC, "<$oc_file") or die "Couldn't open $oc_file!\n";
	while (<OC>) {
		s/[\n\r\s]+?$//g;
		next if /^#/ or !/\w/;
		$cardinal_numbers{$_}++;
	}
	close (OC);
	$oc_initialized = 1;
}

# Was package Tag::TextFunctions;

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

convert_to_speedy(\%xml_subs,	  $xmla,	$xmlr);
convert_to_speedy(\%gloss_subs,	  $glossa);
convert_to_speedy(\%gloss_post,	  $glossr);
convert_to_speedy(\%gloss_undo, $glossu);
convert_to_speedy(\%apos_subs,	  $aposa,	$aposr);
convert_to_speedy(\%number_subs,  $numbera, $numberr);
convert_to_speedy(\%paren_subs,	  $parena,	$parenr);
convert_to_speedy(\%quest_subs,	  $questa,	$questr);

# not exported. Basis for most of the replacement functions in here
sub do_subs {
	my ($aref, $string) = @_;
	for (my $i=0; $i<@$aref; $i+=2) {
		$string =~ s/$aref->[$i]/$aref->[$i+1]/g;
	}
	return $string;
}

sub XML_prep			 { return do_subs($xmla,	@_); }
sub XML_post			 { return do_subs($xmlr,	@_); }
sub Gloss_prep			 { return do_subs($glossa,	@_); }
sub Gloss_post			 { return do_subs($glossr,	@_); }
sub Gloss_safety_measure { return do_subs($glossu,	@_); }
sub Hide_apos			 { return do_subs($aposa,	@_); }
sub Unhide_apos			 { return do_subs($aposr,	@_); }
sub Hide_comma			 { return do_subs($numbera, @_); }
sub Unhide_comma		 { return do_subs($numberr, @_); }
sub Hide_parens			 { return do_subs($parena,	@_); }
sub Unhide_parens		 { return do_subs($parenr,	@_); }
sub Hide_quest			 { return do_subs($questa,	@_); }
sub Unhide_quest		 { return do_subs($questr,	@_); }
	
sub zpad {
	my ($num, $len) = @_;
	return undef if $len <= 0;
	return $num if length($num) > $len;
	return substr("0" x $len . $num, $len * -1);
}

sub hasAttributeValue {
	my ($attr, $value, $elt) = @_;
	return 0 unless defined($value) && defined($attr);
	$value ||= '[^"]*?';
	my $ret = ($elt =~ /^\s*<[^\s\/<>]+?\b[^>]*?\s$attr="(?:$value)"/) ? 1 : 0;
	return $ret;
}

sub getAttributeValue {
	my ($attr, $elt) = @_;
	return undef if !defined($attr) or $elt !~ /^\s*<[^\s\/<>]+?\b[^>]*?\s$attr="/;
	my $ret = ($elt =~ /^\s*<[^\s\/>]+?\b[^>]*?\s$attr="([^"]+?)"/) ? $1 : "";
	return $ret;
}

sub setAttributeValues {
	my ($elt, $attrs) = @_;
	return $elt unless defined $attrs;
	foreach my $key (keys %$attrs) {
		next if !defined($$attrs{$key});
		$elt = deleteAttribute($key, $elt);
		$elt =~ s/^(\s*<[^\s\/>]+?\b)/$1 $key="$$attrs{$key}"/o;
	}
	$elt;
}

sub setContents {
	my ($elt, $text) = @_;
	return $elt unless defined $text;
	$elt =~ s/>[^<]+?(?=<\/)/>$text/;
	$elt;
}

sub deleteAttribute {
	my ($attr, $elt) = @_;
	return $elt unless defined $attr;
	$elt =~ s/^(\s*<[^\s\/>]+?\b[^>]*?)\s$attr=\"([^\"]*?)\"/$1/;
	$elt;
}

sub assign_potential_lemmas {
	my ($wf, $special_handling, $rdf) = @_;
	$special_handling ||= 0;
	$rdf ||= getAttributeValue('rdf', $wf);
	
	$wf = deleteAttribute('lemma', $wf);

	my $word;
	my %attrs;
	if ( defined($rdf) && $rdf ne "" ) {
		$attrs{rdf} = $rdf;
		$word = $rdf;
	} else {
		$wf =~ /^\s*<wf[^<>]*>(.+?)<\/wf>/o;
		$word = $1;
		# Remove trailing punctuation (except period) in preparation
		# for lemmatization (but only strip if does not follow
		# final punctuation other than a period!) 
		$word =~ s/([^:;,!\?\ ])[:;,!\?]\ *$/$1/go;
	}

	my ($l, $s) = get_lemmas_by_moaning($word, $special_handling);
	$attrs{lemma} = $l;
	if (defined $s) {
		# $word is a "yellow-" style hyphenated word, so set
		# contents to the lemma "yellow" and sep= to "-" + sep
		$word = substr($word,0,-length($s));
		$wf = setContents($wf, $word);
		my $sep = getAttributeValue('sep', $wf);
		$sep = " " unless defined $sep;
		$attrs{sep} = join("", $s, $sep);
	}
	if ($split_nonwords && $l !~ /%\d(?:$|\|)/ && $word =~ /[a-z0-9,\.']-[a-z]/i && $word !~ /^[A-Z][^-]*-[A-Z]|^(?:Rh|Kai|Yat|[ae]l)-|\d-\d|^-|-$/) {
		# Split tax-deferred, machine-readable, anti-Semitism, 3-sided,
		# 12-ft, states'-rights, etc.
		# (But not Binet-Simon & Latter-Day, #-# (numeric/date ranges), 
		# 6:9-13, --, CH3CO-, -teen, Rh-positive/negative, or al-Fatah, etc.
		my @parts = split /-/, $word;
		my $split_wf;
		for (my $p = 0; $p <= $#parts; $p++) {
			my ($part_l, $part_s) = get_lemmas_by_moaning($parts[$p], $special_handling);
			# nb: splitting regardless of whether all forms are in wn!
			#return setAttributeValues($wf, \%attrs) unless ($part_l =~ /%\d(?:$|\|)/);
			my %a;
			if (my $stop = is_stoplist($parts[$p])) {
				$a{lemma} = $stop;
				$a{tag} = "ignore";
			} else {
				$a{lemma} = $part_l;
				$a{tag} = "un";
				$a{noauto} = "T" if is_noauto($parts[$p]);
			}
			$a{sep} = "-" if $p < $#parts || @parts == 1;
			$split_wf .= tag_elt($parts[$p], "wf", \%a);
		}
		return $split_wf;
	} else {
		$attrs{noauto} = "T" if is_noauto($word);
		return setAttributeValues($wf, \%attrs);
	}
}

sub get_lemmas_by_moaning {
	my ($word, $special_handling) = @_;
	$special_handling ||= 0;
	my ($lemma, $sep);
	my @word_pos_pairs = ();
	my %pos_map = qw/n 1 v 2 a 3 r 4/;

	$word =~ s/\.\ *$//o if $word !~ /[\ _]/o;
	
	$morph = WordNet::Morphology->new(@defaults, reader => 'Cache') unless defined $morph;

	# First get "special" base forms for non-wn words (eg, the verbs
	# tump and doll)
	@word_pos_pairs = _pre_moan($word, $special_handling);
	# Call moan for potential lemmas/pos of form as is (case-sensitively)
	push @word_pos_pairs, $morph->moan($word);
	if ( $special_handling && $word =~ /^[A-Z]/o && (my @word_pos_pairs_lc = $morph->moan(lcfirst $word)) ) {
		# Append any additional potential lemmas/pos for words that are
		# the first word in a sentence
		@word_pos_pairs = map {$_} @word_pos_pairs, @word_pos_pairs_lc; 
	}

	if (!@word_pos_pairs) {
		if (($word =~ m/^(.+?)\-$/) && (@word_pos_pairs = $morph->moan($1))) {
			# Handle yellow- to yellow-green cases
			while (@word_pos_pairs) {
				my ($form, $ben_pos) = splice @word_pos_pairs, 0, 2;
				my $pos = $pos_map{substr $ben_pos, 0, 1};
				unless ( defined $lemma	 && ( $lemma =~ /(?:^|\|)$form%$pos(?=$|\|)/ ) ) {
					$lemma = defined $lemma ? join("|", $lemma, "$form%$pos") : "$form%$pos";
				}
				$sep = "-";
			}
		} else {
			$lemma = $word;
			$lemma =~ s/\'s$|(?<=s)\'$//o;
			if ( $lemma =~ m/^[A-Z].*s$/) {
				# "Rivers" problem: looks like a pluralized common noun that
				# is capitalized due to being part of a proper collocation;
				# try moaning for lc'd version
				@word_pos_pairs = $morph->moan(lcfirst $lemma);
				while (@word_pos_pairs) {
					my ($form, $ben_pos) = splice @word_pos_pairs, 0, 2;
					$form = ucfirst($form);
					$lemma = join("|", $lemma, $form) unless ( $lemma =~ /(?:^|\|)$form(?=$|\|)/ );
				}
			}
		}		 
	} else {
		# build $lemma="baseform1%pos1|baseform2%pos2|..."
		while (@word_pos_pairs) {
			my ($form, $ben_pos) = splice @word_pos_pairs, 0, 2;
			my $pos = $pos_map{substr $ben_pos, 0, 1};
			unless ( defined $lemma	 && ( $lemma =~ /(?:^|\|)$form%$pos(?=$|\|)/ ) ) {
				$lemma = defined $lemma ? join("|", $lemma, "$form%$pos") : "$form%$pos";
			}
		}
	}
	return ($lemma, $sep);
}

sub assign_pseudolemma {
	my $wf = shift;
	$wf =~ s{<wf.+?</wf>}{deleteAttribute('lemma', $&)}geo;
	$wf =~ s/(<wf[^<>]*)(>(?:<id[^<>]*>)*(.+?))(?=<\/wf>)/$1 lemma="$3"$2/go;
	return $wf;
}

sub build_mwf_lemma {
	my $mwf = shift;
	my $mwf_lemma = format_XML_contents_tf($mwf);
	$mwf_lemma =~ s/ /_/go;
	return $mwf_lemma;
}

sub sense_tag_tf {
	my ($elt, $lemma_sk_pairs, $manorauto, $xml_str) = @_;
	return if !defined($elt);
	die "\$lemma_sk_pairs not defined in sense_tag_tf!" if !defined($lemma_sk_pairs);
	die "\$manorauto not defined!" if !defined($manorauto);
	die "\$xml_str not defined!" if !defined($xml_str);

	# Start with the next id-num in sequence...
	my $id_num = next_id_tf('id-num', $xml_str);
	
	my $wf_lemma;
	my $cnt;
	while (@$lemma_sk_pairs) {
		my $lemma = shift @$lemma_sk_pairs;
		my $sk = shift @$lemma_sk_pairs;
		my $id = "<id/>";
		$id = setAttributeValues($id, {'id-num' => $id_num, 'sk' => $sk, 'lemma' => $lemma});
		$elt =~ s/^\s*<[^<>\/]+?>/$&$id/o;

		if ( !defined($wf_lemma) ) {
			$wf_lemma = $lemma;
		} elsif ( $wf_lemma !~ /(?:^|\|)$lemma(?:[\|%]|$)/ ) {
			$wf_lemma = join("|", $wf_lemma, $lemma);
		}

		$id_num++;
		$cnt++;
	}
	$manorauto = "un" if $cnt > 1;	  
	return setAttributeValues($elt, {lemma => $wf_lemma, tag => $manorauto});
}

sub format_XML_contents_tf {
	my ($str, $use_lemmas) = @_;
	$use_lemmas ||= 0;

	$str =~ s/<id[^>]+?>//go;
	# This won't work for embedded qf's...
	$str =~ s/<qf[^>]*?rend="sq"[^>]*>(.+?)<\/qf>/\`$1\' /go;
	$str =~ s/<qf[^>]*?rend="dq"[^>]*>(.+?)<\/qf>/"$1" /go;
	
	my $ppstr;
	my $sep = " ";
	while ( $str =~ m/<wf[^<>]*>(.+?)<\/wf>/go ) {
		my $wf = $&;
		my $contents = $1;
		my $lemma;
		if ( $use_lemmas && (my $attval = getAttributeValue('lemma', $wf)) ) {
			my %lemmas;
			$attval =~ s/^(.+?)(?=$|%\d+)/$lemmas{$1}++/geo;
			foreach my $l (keys %lemmas) {
				$lemma = defined($lemma) ? "$lemma\|$l" : $l;
			}
		}
		$lemma ||= $contents;
		# The following is not really necessary anymore, since all 
		# type="punc" wfs should have sep="" attribute, but may as 
		# well keep in to be on the safe side
		$sep = "" if hasAttributeValue('type', "punc", $wf) && $wf =~ />[:;,!\.\?\)]/o;
		$ppstr = !defined $ppstr ? $lemma : "$ppstr$sep$lemma";
		$sep = hasAttributeValue('sep', "", $wf) ? getAttributeValue('sep', $wf) : " ";
	}
	return $ppstr;
}

sub _strip_pos {
	my $tmp = shift;
	$tmp =~ s/%\d+?$//o;
	return $tmp;
}

sub tag_elt {
	my ($contents, $gi, $attrs) = @_;
	if ( !defined $attrs ) {
		return "<$gi>$contents</$gi>";
	} else {	   
		return setAttributeValues("<$gi>$contents</$gi>", $attrs);
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

our %contractions = ();
{
	$contractions{"I'm"}   = [	  "I",	"I",  "am",	 "'m" ];
	$contractions{"ain't"} = [	 "be", "ai", "not", "n't" ];
	$contractions{"can't"} = [	"can", "ca", "not", "n't" ];
	$contractions{"cannot"} = [	 "can", "can", "not", "not" ];
	$contractions{"won't"} = [ "will", "wo", "not", "n't" ];
	for my $word (qw/I it they we you he she/) {
		$contractions{$word."'d"}	  = [ $word, $word,
										  "have|do|would|should", "'d" ];
		$contractions{$word."'ll"}	  = [ $word, $word, "will", "'ll" ];
		$contractions{$word."'ll've"} =
			[ $word, $word, "will", "'ll", "have", "'ve" ];
	}
	for my $word (qw/they we you/) {
		$contractions{$word."'re"}	  = [ $word, $word, "are", "'re" ];
	}
	for my $word (qw/I they we you/) {
		$contractions{$word."'ve"}	  = [ $word, $word, "have", "'ve" ];
		$contractions{$word."'d've"}  =
			[ $word, $word, "would", "'d", "have", "'ve" ];
	}
	for my $word (qw/are could dare do does did had has have
				  is must need should was were would/) {
		$contractions{$word."n't"}	  = [ $word, $word, "not", "n't" ];
	}	 
	for my $word (qw/he here it she that there what where/) {
		$contractions{$word."'s"}	  = [ $word, $word, "is", "'s" ];
	}
	$contractions{"let's"}	  = [ "let", "let", "us", "'s" ];
	$contractions{"oughtn't"} = [ "ought", "ought", "not", "n't" ];
	$contractions{"y'all"}	  = [ "you", "y'", "all", "all" ];
	$contractions{"'tis"}	 = [ "it", "'t", "is", "is" ];

	$contractions{lc $_}=$contractions{$_} for grep /[A-Z]/,keys %contractions;
}
my %general_ending = ();
{
	$general_ending{"'d"}  = [ "have|do|would|should", "'d" ];
	$general_ending{"'ll"} = [ "will", "'ll" ];
	$general_ending{"'re"} = [ "be",  "'re" ];
}

sub decontract {
	local $_ = shift;
	return () if $_ ne 'cannot' and -1 == index $_, "'";
	return () if /^(.+)\'e?d$/ and is_abbreviation($1);
	my $lc = lc;
	return _matched_case($_, $contractions{$lc}) if defined $contractions{$lc};
	for my $e (keys %general_ending) {
		if ($e eq substr $_, -length($e)) {
			(my $base = $_) =~ s/$e$//;
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

# Was package Tag::IndexFunctions;

#TODO: put code in get_all_forms() for upper and lower case versions of small words,
#	   prefixes, and the lc'd version of ucletter-lcword, eg., X-ray & x-ray
#TODO: generate all permutations of all forms of cfs for collocs!

my %fn_word_list_by_value = ();
my ($hyp_prefixes, $hyp_misc);

sub get_all_forms {
	my ($lemma, $strip_punc) = @_;
	$strip_punc ||= 0;

	# Strip punctuation--the wf_index does not retain punctuation,
	# except for abbreviations.
	$lemma = _normalize($lemma) if $strip_punc;

	my %all_forms = ();
	if ( $lemma !~ /\-/o ) {
		%all_forms = map { $_ => 1 } dereverse_hyphenated(lc $lemma);
	} else {	
		my @forms_thusfar;
		my @words = split ' ', $lemma;
		foreach my $word (@words) {
			if ( $word =~ /\-/o ) {
				# \x09 = close up/keep, \x7F = space replace/keep,
				# "-", \x0D = keep/close up/space replace

				# Do subs for misc items for which want to pre-empt replacing "-"
				# For example: ibn-M..., al-M..., der-S, etc.
				foreach my $misc (sort {length($b) <=> length($a)} keys %$hyp_misc) {
					$word =~ s/(?=^|\b)($misc)(?=$|\b)/_do_subs($1,"-","\x0D")/e;
				}
				
				# Find and do subs for prefixes that can close up, but
				# not stand alone
				foreach my $pfx (sort {length($b) <=> length($a)} keys %$hyp_prefixes) {
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
				$word =~ s/([a-z]\-[A-Z])/_do_subs($1,"-","\x7F")/geo;
				
				# Don't close up 1st-, 2nd-, 3rd-, etc.
				$word =~ s/((?:1st|2nd|3rd|\dth)\-)/_do_subs($1,"-","\x7F")/geo;
				
				# Don't close up number followed by lower-cased word,
				# or a number (so disallows 12tone, 3membered, 911 for
				# 9-11, but allows 401k and 3D.)
				$word =~ s/(\d\-)(?=([a-z]{2,}|\d))/_do_subs($1,"-","\x7F")/geo;

				# After words ending with 's or ', or before words beginning with '
				$word =~ s/((?:\-')|(?:'s?\-))/_do_subs($1,"-","\x7F")/geo;

				# Now determine all versions based on subs
				
				my @versions_this_word;

				# Plain old hyphens get 3 forms: with hyphen, space instead, closed up
				$word =~ s/\x0D/-/go;
				push @versions_this_word, $word;
				if ( $word =~ /\-/o ) {
					push @versions_this_word, $word, $word;
					$versions_this_word[$#versions_this_word] =~ s/\-//go;
					$versions_this_word[$#versions_this_word-1] =~ s/\-/ /go;
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
	
	$all_forms{$lemma}++;
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
		# Skip tag="ignore" aux's & classif, which will not be tagged
		$str =~ s/^\s*<aux[^>]* tag="ignore"[^>]*>.*?<\/aux>\s*?(?=$|<)//o && do {$new_str .= $&; redo;};
		$str =~ s/^\s*<classif[^>]*>.*?<\/classif>\s*?(?=$|<)//o && do {$new_str .= $&; redo;};
		# Skip start/end tags except for wfs
		$str =~ s/^\s*<\/?(?!wf)[^>]*?>\s*?(?=$|<)//o && do {$new_str .= $&; redo;};
		$str =~ s/^<wf.+?<\/wf>\s*?(?=$|<)//o && do {$new_str .= _set_wf_id_num($&, $next_id); redo;};
	}
	return $new_str;
}

sub next_id_tf {
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
	return setAttributeValues($wf, {'wf-num' => $_[1]++});
}

# Normalize index form of a lemma--strip periods if is not an
# abbreviation, strip all other punctuation.
sub _normalize {
	my $word = shift;
	$word =~ s/[:;,!\(\)\?]//go;
	$word =~ s/\.(?!\d)//go if !is_abbreviation($word);
	return $word;
}

sub index_info_formatted_for_sql {
	my ($str, $synset_id, $which_field, $wfi_ref, $stag_ref) = @_;
	$wfi_ref ||= []; 
	$stag_ref ||= [];
 
	if ( !defined($which_field) || $which_field !~ /^(?:gloss|examples)/o ) {
		return () unless $str =~ /<gloss[^>]*?>(.+?)<\/def>/;
		($wfi_ref, $stag_ref) = index_info_formatted_for_sql($1, $synset_id, "gloss", $wfi_ref, $stag_ref);
		if ( $str =~ /(<\/def>(?:<aux.+?<\/aux>)*)(<ex.+?)(?=<\/gloss)/o ) {
			($wfi_ref, $stag_ref) = index_info_formatted_for_sql($2, $synset_id, "examples", $wfi_ref, $stag_ref);
		}
	} else {
		# process all <[cw]f>'s with lemmas and id's
		while ( $str =~ s/<([cw]f)[^>]*?>.+?<\/\1>// ) {
			my $wf = $&;
			my $lemma = getAttributeValue("lemma", $wf);
			my $wf_num = getAttributeValue("wf-num", $wf);
			my $tagtype = getAttributeValue("tag", $wf);

			next if !defined($lemma) || $tagtype eq "ignore" || !defined($wf_num);

			# wf_index
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

			# sense_tags index
			while ( $wf =~ s/<id[^>]*?>//o ) {
				# process <id/>'s for this wf...
				my $id = $&;
				my $sk = getAttributeValue("sk", $id);
				my $id_num = getAttributeValue("id-num", $id);
				# Every <id/> has exactly one lemma, so no need
				# to a) check if exists, and b) split out multiples
				my $lemma = getAttributeValue("lemma", $id);
				$lemma =~ s/%\d+?$//o;
				$lemma = _normalize($lemma);
				push @$stag_ref, "$id_num\t$lemma\t$synset_id\t$tagtype\t$sk";
			}
		}		 
	}
	return ($wfi_ref, $stag_ref);
}

sub _initIx () {
	$hyp_prefixes = _loadExFile($px_file);
	$hyp_misc = _loadExFile($hm_file);
	_loadFwords();
	$ix_initialized = 1;
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
	my %fn_words = ();
	while (<FIL>) {
		next if m/^#/;
		s/[\n\r\s]+?$//g;
		my $cnt;
		(my $str = $_) =~ s/\-/++$cnt/ge;
		$fn_words{$_} = $cnt;
	}
	while ( my ($key, $value) = each %fn_words) {
		push @{$fn_word_list_by_value{$value}}, $key;
	}
	close (FIL);
}

sub _load_special {
	open (FIL, "<$sp_file") or die "Couldn't open $sp_file!\n";
	while (<FIL>) {
		next if m/^#/;
		s/[\n\r\s]+?$//g;
		my @word_pos_pairs = split /\|/;
		my ($base_form, $base_pos) = splice @word_pos_pairs, 0, 2;
		push @{$unmoanable_nonwords{$_}}, ($base_form, $base_pos) for (@word_pos_pairs);
	}
	close (FIL);
}

sub _pre_moan {
	my ($word, $special_handling) = @_;
	$special_handling ||= 0;
	return @{$unmoanable_nonwords{$word}} if defined $unmoanable_nonwords{$word};
	return @{$unmoanable_nonwords{lcfirst $word}} if defined $unmoanable_nonwords{lcfirst $word} && $special_handling;
	return ();
}

# Was package Tag::Abbreviations;

# %abbrevs hash holds all abbreviations & acronyms in wn to be sense-tagged 
# automatically. It is keyed by the actual form as found in the text.
# The value of the hash is a hash containing the sense key(s), type and
# optional rdf. Sense key contains an array ref, either to an array of
# sense key(s), or to an array of hash refs of sense key dependencies.
# Structure is:
#	 $abbrevs{$wordform}->{form} (required)
#
# and
#	 $abbrevs{$wordform}->{type} (=abbr or acronym)
#	 $abbrevs{$wordform}->{rdf} (optional)
#	 $abbrevs{$wordform}->{sk}->[0] (only one sense key)
# or
#	 $abbrevs{$wordform}->{sk}->[0] (first sense key)
#	 $abbrevs{$wordform}->{sk}->[1] (second sense key)
#				 .
#				 .
#				 .
# or
#	 $abbrevs{$wordform}->{sk}->[0]->{sk}	  (Assign this sense key...)
#	 [$abbrevs{$wordform}->{sk}->[0]->{prev/nextword} (but only if prev/next word
#													   matches this)]
#	 $abbrevs{$wordform}->{sk}->[1]->{sk}	  (Assign this sense key...)
#	 [$abbrevs{$wordform}->{sk}->[1]->{prev/nextword} (but only if prev/next word
#													   matches this)]
#				 .
#				 .
#				 .
# or
#	 $abbrevs{$wordform}->{sk}->[0]->{sk}	  (Assign this sense key...)
#	 [$abbrevs{$wordform}->{sk}->[0]->{prev/nextword} (but only if prev/next word
#													   matches this)]
#	 $abbrevs{$wordform}->{sk}->[1]->{sk}	  (else assign this sense key...)

my %abbrevs = ();

sub is_abbreviation {
	my ($word, $prevword, $nextword) = @_;
	$prevword ||= "";
	$nextword ||= "";
	$word =~ s/\'[sd]$//o || $word =~ s/(?<=[A-Z])s$//o unless !defined $word;
	return () if !defined $word || !defined $abbrevs{$word}->{form};

	my $lemma_form = defined $abbrevs{$word}->{rdf} ? $abbrevs{$word}->{rdf} : $word;
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
				if ( defined $sk_inf->{prevword} || defined $sk_inf->{nextword} ) {
					# Is some kind of dependency, check if valid
					my $dependent_on = defined $sk_inf->{prevword} ? $prevword : $nextword;
					my $regex = defined $sk_inf->{prevword} ? qr{$sk_inf->{prevword}} : qr{$sk_inf->{nextword}};					
					next if $dependent_on !~ /$regex/;
					push @lemma_sk_pairs, $lemma_form, $sk_inf->{sk};
					last;
				}
				push @lemma_sk_pairs, $lemma_form, $sk_inf->{sk};
			} else {
				push @lemma_sk_pairs, $lemma_form, $sk_inf;
			}
		}
		# If wf is on the list of abbrevs but fails the tests for 
		# dependencies, will return with no @lemma_sk_pairs. In that case,
		# Tag_abbr.pl will do nothing, making no assumptions as to whether
		# it is in fact an abbrev.
		return $lemma_form, $abbrevs{$word}->{rdf}, $abbrevs{$word}->{type}, \@lemma_sk_pairs;
	} else {
		# wf is on the list of abbrevs but is not in WordNet. These
		# should be set to tag="ignore".
		return $lemma_form, $abbrevs{$word}->{rdf}, $abbrevs{$word}->{type};
	}
}

sub _loadAbbrevs () {
	open (ABBREV, "<$abbr_file") or die "Couldn't open $abbr_file!\n";
	while (<ABBREV>) {
		next if m/^\#/;
		s/[\n\r\s]+?$//g;
		next unless /[^\s]/;
		my ($abb, $rdf, $ty);
		my @sns_keys;

		# Parse line
		s/^([^,]+?),([^,]+?)(?:$|,)//;
		$abb = $1;
		$ty = $2;
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
		} elsif ( /^,?\{sns_key=[^,]+?,(?:prev|next)word=/ ) {
			# Some flavor of dependency (if-then, or if-then-else)
			my $href;
			while ( s/^,?\{sns_key=([^,]+?)(,((?:prev|next)word)=[^\}]*?)?\}(?:$|\&|<=>)// ) {
				my $sk = $1;
				my $pn_word = $2;
				my $whichisit = $3;
				if ( defined($pn_word) ) {
					$pn_word =~ /^,$whichisit=(.+?)$/;
					push @sns_keys, {sk => $sk, $whichisit => $1};
				} elsif ( $sk !~ /}&{/o ) {
					push @sns_keys, {sk => $sk};
				} else {
					while ( $sk =~ s/^\{sns_key=([^\}]+?)\}(?:$|\&)//o ) {
						push @sns_keys, {sk => $1};
					}
					warn "$abbr_file corrupt at line $., sk=$sk" if $sk ne "";
				}
			}
		}
		warn "$abbr_file corrupt at line $." if $_ ne "";

		$abbrevs{$abb}->{form} = $abb;
		$abbrevs{$abb}->{type} = $ty;
		$abbrevs{$abb}->{sk} = \@sns_keys if @sns_keys;
		$abbrevs{$abb}->{rdf} = $rdf if defined($rdf);
	}
	close (ABBREV);
	$abbr_initialized = 1;
}

1;

__END__

=head1 NAME

Combined module for Tag::TextFunctions, Tag::IndexFunctions and Tag::Abbreviations

=head1 DESCRIPTION

Various functions for dealing with text-based XML manipulation, indexing and abbreviation-handling. 

=head1 Functions for Tag::TextFunctions

=over 4

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

=item B<zpad(>I<num>, <length>B<)>

Pad $num with leading zeros to specified length string. E.g., zpad(35, 6) will return "000035". $num is assumed to be a number, and $len a number greater than zero. The only check performed are that $len is >= 0 and the length of $num is <= $len.

=head1 XML-ish Functions

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

=item B<assign_potential_lemmas(>I<wf>, I<special_handling>, I<rdf>B<)>

If lemma= attribute not already set, determines and assigns all posited lemma/pos pairs as returned from moan. If $special_handling is non-zero, then this is the first word of an example sentence, and so might be capitalized. 

If a redefinition value is passed in, it is used as the word to be lemmatized instead of the tag contents, and rdf= attribute is assigned.

Do not use this routine to assign lemma for ignorable text (which does not get lemmatized), or for abbreviations (which may have trailing punc, and for which sense keys are predetermined)--use assign_pseudolemma for the former and setAttributeValues for the latter instead.

nb. lemma differs from the sense_key form in that case is retained!
	
=item B<assign_pseudolemma(>I<wf>B<)>

Sets lemma= attribute on the passed-in <wf> (or string of <wf>s) to the contents of each <wf>, without actually lemmatizing the contents. Only to be used for ignorable text, as it does no wn lookup, and thus makes no attempt to determine pos.

Side effect: overrides current lemma=, if any.

=item B<build_mwf_lemma(>I<mwf>B<)>

Builds lemma based on contents of constituent wfs. Defaults to "_" as concatenator char, unless sep= value is present in the previous wf (in which case that value is used).

note: This will work for mwf's that are either wrapped in <mwf>...</mwf> tags or just a string of wfs not yet tagged as mwf.

=item B<format_XML_contents_tf(>I<tagged_text>B<)>

Removes XML tags from a tagged string of text and restores spacing based on sep= attribute.

=item B<sense_tag_tf(>I<(wf|mwf)>, I<lemma_sk_pairs>, I<manorauto>, I<xml_str>B<)>

Sense tag the passed in wf or mwf. This consists of creating an empty <id/> tag for each sk/lemma pair, setting sk=, lemma=, and id_num= on it, and inserting it into the wf/mwf just after the start tag.

Arguments:

$wf/$mwf is the element to be tagged

$lemma_sk_pairs is an array ref of a list of lemma/sense key tuples

$manorauto should be either "auto" or "man". If more than one sense_key is being assigned, then the tag= attribute gets reset to "un", overriding the passed-in value.

$xml_str is the entire XML-tagged gloss. It is used to find the next id-num in sequence.
   
=item B<tag_elt(>I<contents>, I<tag>, I<$href>B<)>

Wraps $contents in start/end tags. Assigns attributes and their values if passed in as key/value pairs in the hash ref'd by $href.

note: Routine allows for null attribute values, so that sep="" can be assigned.

=item B<wrap_synset_elts(>I<$gloss>, I<$examples>B<)>

Wraps $gloss in <gloss>...</gloss> tags. If $examples are not null, then wraps them in <examples>...</examples> tags. Finally, wraps the entire thing in <synset>...</synset> tags. The <synset> tag becomes the root gi (=tag) for the Twig object.

=head1 Functions for Tag::IndexFunctions

=item B<get_all_forms(>I<$lemma>B<)>

Takes passed-in lemma form and returns an array of all forms of the
word/collocation in terms of hyphenation, spacing, and
punctuation differences. Certain words will be returned in both upper
and lower case.

e.g., about-face, about face, aboutface; ad-lib, adlib, ad lib; re-examine,
reexamine; ram's-head, ram's head; Shih-Tzu, Shih Tzu; Robert E Lee (minus the period)

No attempt is made to insert spaces/hyphens into juxtaposed forms, only
to determine variants where there are hyphens in the input form.

=item B<assign_ids_to_wfs(>I<$xml_tagged_string>, I<$next_id>B<)>

Takes passed-in text and assigns id numbers to all ID-able wfs, starting
at $next_id. <wf>s that are within ignorable <aux>'s are skipped, since
these wfs will never be auto/manually tagged. All other wfs are passed to
_set_wf_id_num() whether ignorable or not (since some ignorable wfs go into the index).

The passed-in text should be tokenized (wf-tagged) but no ids on wfs should be
assigned yet, as they will be overridden here. id-num's on <id/>'s are
not touched.

If you are not id'ing the entire gloss at once, pass in the return value of
Tag::Functions::next_id_tf(wf-num, $xml_tagged_string) as the initial $next_id,
otherwise the id numbers will start over again! For example, to id gloss and
example wfs separately, do something like:

	s/(<gloss[^>]*?>)(.+?)(?=<\/def)/$1 . assign_ids_to_wfs($2,1)/e;
	s/(<\/def[^>]*?>)(.+?)(?=<\/gloss)/$1 . assign_ids_to_wfs($2, &next_id_tf("wf-num", $_))/e;
	
=item B<next_id_tf(>I<$attr>, <$xml_tagged_string>B<)>

Takes passed-in xml-tagged text and determines the next id number in
sequence for the indicated attribute. This will be the largest value
found for that attribute plus 1.

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
	  
=head1 Tag::Abbreviations Functions

=item B<is_abbreviation>I<$word>, <$prevword>, <$nextword>B<)>

Simple test of whether the word is an abbreviation based on orthographic form and previous or following word in the gloss. Returns values are:

=over 4

() - $word is not an abbreviation

($lemma_form, $rdf, $type) - Is an abbreviation, but it is not an entry in wn. These should be set to tag="ignore", unless noauto="T" (the word should be looked at manually)

($lemma_form, $rdf, $type, $lemma_sk_pairs) - Is an abbreviation that is also an entry in wn. $lemma_sk_pairs is an array ref of lemma/sense keys, each pair representing one sense tag. How the abbreviation is handled depends on what is passed back for $lemma_sk_pairs. If the array referenced is empty, then the $word is on the abbreviation list, but failed all criteria, so may not be an abbrev at all (so, do not tag or do anything with the word at all). If the array referenced contains exactly 2 elements, then one sense tag was found, and the abbreviation can be auto-sense tagged via sense_tag_tlc(). If the array referenced contains more than 2 elements, then more than one sense tag was found, and it should not be auto-tagged. Rather, all lemmas should be assigned, and noauto= set to "T" so that they can be looked at manually later.
	

