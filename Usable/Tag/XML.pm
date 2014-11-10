use 5.006;
use strict;
use warnings;

package Tag::XML;
use base qw/Exporter/;
our @EXPORT = qw/%sql_fields %sql_id
    %sql_insert %sql_select
    @sql_rebuild_order/;

our %sql_fields =
    ( gloss => [ qw/id synset_key pre gloss aux examples post/ ],
      lemma_index => [ qw/gloss_id wf_num which_field lemma tag_type/ ],
      sense_tags => [ qw/gloss_id wf_num coll id_num lemma tag_type sense_tag user time/ ],
      globs => [ qw/gloss_id wf_nums coll lemma glob user time/ ],
      rdfs => [ qw/gloss_id wf_num rdf lemma user time/ ],
     );
our %sql_id = map { $_ => $sql_fields{$_}[0] } keys %sql_fields;
our %sql_insert = map {
    $_ => 'insert into '.$_.
        ' ('.join(',', @{$sql_fields{$_}}).') '.
        'values ('.join(',', ('?') x @{$sql_fields{$_}}).')';
} keys %sql_fields;
our %sql_select = map {
    $_ => 'select '.join(',', @{$sql_fields{$_}}).
        ' from '.$_;
} keys %sql_fields;
our @sql_rebuild_order = qw/rdfs globs sense_tags/;

use XML::Twig;

sub new { shift; XML::Twig->new(keep_encoding => 1, @_); }

package XML::Twig::Elt;

use Tag::Functions;

sub head_glob {
	my $elt = shift;
	return unless $elt->gi eq 'cf';
	my ($glob) = $elt->children('glob');
	return $glob if $glob;
	return unless my ($coll) = split /,/, ($elt->att('coll') || '');
	return unless ($glob) = $elt->root->descendants(qq{glob[\@coll="$coll"]});
	$glob;
}

sub just_text {
    my ($e) = shift;
    my @elts = ($e);
    while (grep { ref ne 'ARRAY' } @elts) {
        @elts = map {
            ref eq 'ARRAY' ? $_
                : $_->gi eq 'qf'
                ? ([ $_ ], $_->children('#ELT'), [ $_ ])
                : [ $_ ];
        } @elts;
    }
    my $text = '';
    $text .= ($_->gi eq 'qf' ? ($_->att('rend') eq 'sq' ? "'" : '"')
              : $_->gi =~ /^[wc]f$/
              ? ($_->text . (defined($_->att('sep')) ? $_->att('sep') : ' '))
              : '') for @elts;
}

my %part_nums = qw/1 n 2 v 3 a 4 r 5 a/;
sub lemma_pos_pairs {
    my @lemmas = split /\|/, (shift->att('lemma') || '');
    @lemmas = map { (/^(.*)\%(\d)$/ and $part_nums{$2})
                        ? [ $1, $part_nums{$2} ]
                        : [ $_ ] } @lemmas;
    my %seen; @lemmas = grep { not $seen{join('|',@$_)}++ } @lemmas;
}

sub raw_lemmas { my %s; grep !$s{$_}++, split /\|/, shift->att('lemma')||''; }

sub lemmas {
    my @lemmas = split /\|/, (shift->att('lemma') || '');
    s/(?<=.)\%.*$// for @lemmas;
    my %seen; @lemmas = grep { not $seen{$_}++ } @lemmas;
}

sub tag_type {
    my $elt = shift;
    my $tag = $elt->att('tag') || '';
    my $has_id = $elt->descendants('id') ? 1 : 0;
    $tag = 'ignore' unless $elt->is_taggable;
    $tag = 'partial' if $tag eq 'un' and $has_id;
    $tag;
}

sub rebuild_from_indices {
    my ($self, $index_info) = @_;
    # untag everything
    $_->sense_tag_2k4('man', ($_->att('coll') || ''), 0) for grep { $_->children('id') } map { $self->descendants($_) } qw/glob wf/;
    # unglob everything
    my %seen;
    $self->unglob($_) for grep { not $seen{$_}++ } map { $_->att('coll') } $self->descendants('glob');
    
    for my $t (@sql_rebuild_order) {
        next unless my @rows = @{$index_info->{$t}};
        $_->[$#$_] =~ tr/0-9//dc for @rows; # strip punctuation from times
        shift @$_ for @rows; # delete gloss_id
        if ($t eq 'rdfs') {
# rdfs:       gloss_id wf_num rdf lemma user time
            for my $r (@rows) {
                my ($wfnum, $rdf, $lemma, $user, $time) = @$r;
                my ($wf) = $self->descendants(qq/[\@wf-num="$wfnum"]/);
                unless ($wf) { warn "no such wfnum: $wfnum\n"; next; }
                $wf->rdf_2k4($rdf, $lemma, $user);
                $wf->set_att('time', $time);
            }
        } elsif ($t eq 'globs') {
# globs:      gloss_id wf_nums coll lemma glob user time
    # glob: globtype lemmas wfnums (user)
            for my $r (sort { $a->[1] cmp $b->[1] } @rows) { # sort by 'coll'
                my ($wfnums, $coll, $lemma, $globtype, $user, $time) = @$r;
                (warn "Couldn't glob?\n" and next) unless my $glob = $self->glob($globtype, $lemma, split(/,/, $wfnums), $user);
                my $got = $glob->att('coll')||'';
                warn "Different coll attribute ($got <> $coll)\n"
                    unless $got eq $coll;
                $glob->set_att('time', $time);
            }
        } elsif ($t eq 'sense_tags') {
# sense_tags: gloss_id wf_num coll id_num lemma tag_type sense_tag user time
    # sense_tag_2k4: tagtype coll append lemmaskpairs (user)
            for my $r (sort { $a->[2] <=> $b->[2] } @rows) {
                my ($wfnum, $coll, $id_num, $lemma, $tag_type, $sense_tag, $user, $time) = @$r;
                my ($wfglob) = $coll
                    ? $self->descendants(qq/glob[\@coll="$coll"]/)
                    : $self->descendants(qq/wf[\@wf-num="$wfnum"]/);
                $wfglob->sense_tag_2k4($tag_type, $coll, 1, $lemma, $sense_tag, $user);
                my ($idtag) = $wfglob->descendants(qq/id[\@id-num="$id_num"]/);
                unless ($idtag) { warn "different ID? Not updating time\n"; next; }
                $idtag->set_att('time' => $time);
            }
        } else {
            warn "Unknown table $t. Skipping.\n";
        }
    }
}

sub sql_index_info {
    my ($self, $gloss_id, $no_gloss_info) = @_;
    my @gloss_id = (defined $gloss_id ? ($gloss_id) : ());

    my %data;
    for ( [ 'def', 'gloss'    ],
          [ 'ex',  'examples' ],
          ) {
        my ($w, $sql_name) = @$_;
        # a push of a grep of a map of a map of a grep of a map... fun!
        push @{$data{lemma_index}},
        grep { $_->[-1] ne 'ignore'; } # don't index ignores
        map { combinations($_); }      # comb's of lemmas and tagtypes: cf's
        map { my $wf = $_;
              [ @gloss_id,
                $wf->att('wf-num'),
                $sql_name,
                [ $_->lemmas ],
                ($wf->gi eq 'wf'
                 ? $wf->att('tag')
                 : [ map { $_->att('tag'); }
                     map { $wf->parent->descendants('glob[@coll="'.$_.'"]'); }
                     split /,/, $wf->att('coll') ]),
                ]; }
        grep { $_->att('lemma') and $_->att('wf-num'); }
        map { my $t = $_;
              map { $t->descendants($_); } qw/wf cf/; } $self->descendants($w);
    }

    @{$data{sense_tags}} = map {
        my $id = $_;
        [ @gloss_id,
          (map { $id->parent($_) } qw/wf cf/)[0]->att('wf-num'),
          $id->att('coll') || '',
          map($id->att($_), qw/id-num lemma/),
          $id->parent->att('tag'),
          $id->att('sk'),
          $id->att('user') || 'auto',
          $id->att('time') ];
    } $self->descendants('id');

    @{$data{globs}} = map {
        my $glob = $_;
        my $friends = 'cf[@coll=~/\b'.$glob->att('coll').'\b/]';
        [ @gloss_id,
          join(',', map($_->att('wf-num'),
                        $self->descendants($friends))),
          map($glob->att($_), qw/coll lemma glob/),
          $glob->att('user') || 'auto',
          $glob->att('time') ];
    } sort {
        $a->att('coll') cmp $b->att('coll');
    } $self->descendants('glob');

    @{$data{rdfs}} = map {
        my $rdf = $_;
        [ @gloss_id,
          map($rdf->att($_), qw/wf-num rdf lemma/),
          $rdf->att('user') || 'auto',
          $rdf->att('time') ];
    } grep {
        not $_->ancestors('aux[@tag="ignore"]');
    } $self->descendants('[@rdf]');

    return (%data) if $no_gloss_info;

    my @parts = @{$sql_fields{gloss}};
    shift @parts; # may or may not have gloss_id - handled below
    my ($xml_gloss) = $self->children('gloss');
    my (@children) = $xml_gloss->children;
    my %gloss = map { $_ => '' } @parts;
    $gloss{synset_key} = $self->att('synset-key');
    $gloss{pre}  = $self->start_tag . $xml_gloss->start_tag;
    $gloss{post} = $xml_gloss->end_tag . $self->end_tag;
    for ( [ 'pre', qr/aux|classif/ ],
          [ 'gloss', qr/def/ ],
          [ 'aux', qr/aux/ ],
          [ 'examples', qr/ex/ ],
          ) {
        my ($v, $qr) = @$_;
        $gloss{$v} .= shift(@children)->sprint
            while @children and $children[0]->gi =~ $qr;
    }
    warn "Extraneous information on gloss: (@{[map($_->sprint,@children)]})\n"
        if @children;
    @{$data{gloss}} = ( [ @gloss_id, map { $gloss{$_} } @parts ] );
    %data;
}


sub is_in_ignore { shift->ancestors('[@tag="ignore"]') ? 1 : 0; }

sub is_taggable {
    my $self = shift;
    return 0 unless $self->gi =~ /^glob|wf$/;
    return 0 unless defined $self->{att}{tag};
    return 0 unless defined $self->{att}{'wf-num'} or defined $self->{att}{coll};
    return 0 if grep { $_->gi ne 'cf' } $self->ancestors('[@tag="ignore"]');
    1;
}

sub is_in_ex { shift->ancestors('ex') ? 1 : 0 }

# TO DO: lemma= on <wf>/<cf> should never change!

BEGIN {
    use Tag::Functions @_;
    use Tag::Stoplist @_;
}

sub rdf_2k4 {
    my $e = shift;
    my ($rdf, $lemma, $user) = @_;
    $user ||= 'auto';
    $e->set_att(rdf => $rdf);
    $e->set_att(lemma => $lemma);
    $e->set_att(user => $user);
    $e->set_att(time => timestamp);
}

our ($_delete_old, $_mantag_debug, $_overt_hits, $_words_between);
BEGIN {
    ($_delete_old, $_mantag_debug, $_overt_hits, $_words_between) = (1,1,0,2);
}
sub sense_tag_2k4 {
    my $e = shift;
    my ($tagtype, $coll, $append, @lemma_sk_pairs) = @_;
    my $user = 'auto';
    $user = pop @lemma_sk_pairs if @lemma_sk_pairs % 2;
    unless ($append) {
	$_delete_old ? $_->cut : $_->set_gi('old') for $e->children('id');
    }
    my %ids = map {
	$_->att('id-num') => 1;
    } $e->parent('gloss')->descendants('id');
    my $id = 1;
    while (my ($lemma, $sk) = splice @lemma_sk_pairs, 0, 2) {
        $id++ while $ids{$id};
	my $idtag =
	    XML::Twig::Elt->new(id =>
				{
				    'id-num' => $id++,
				    lemma => $lemma,
				    sk => $sk,
				    grep($coll, coll => $coll),
                                    user => $user,
                                    time => timestamp
                                },
				'#EMPTY');
	$idtag->paste($e);
    }
    $tagtype = 'un' if $tagtype eq 'man' and not $e->children('id');
    $e->set_att(tag => $tagtype);
}

sub unglob {
    my $e = shift;
    $e = $e->child('gloss') if $e->gi eq 'synset';
    $e = $e->parent('gloss') unless $e->gi eq 'gloss';
    my ($coll) = @_;
    my $search = '[@coll=~/\b'.$coll.'\b/]';
    my @cand = $e->descendants($search);
    my @glob = grep { $_->gi eq 'glob' } @cand;
    my @cfs = grep { $_->gi eq 'cf' } @cand;
    $_->delete for @glob;
    for (@cfs) {
        my %colls = map { $_ => 1 } grep { $_ ne $coll } split /,/, $_->att('coll') || '';
        if (keys %colls) {
            $_->set_att(coll => join(',', sort keys %colls));
        } else {
            $_->set_gi('wf');
            $_->del_att('coll');
            $_->delete for $_->children('id');
        }
    }
}

sub glob {
    my $e = shift;
    $e = $e->child('gloss') if $e->gi eq 'synset';
    $e = $e->parent('gloss') unless $e->gi eq 'gloss';
    my ($globtype, $lemmas, @wfnum) = @_;
    my $user = 'auto';
    $user = pop @wfnum unless $wfnum[$#wfnum] =~ /^\d+$/;
    my @wfs = map {
        $e->descendants('[@wf-num="'.$_.'"]');
    } sort { $a <=> $b } @wfnum;
    die "Couldn't find all wfs: (@{[map { $_->sprint } @wfs]}) != (@wfnum)" if @wfs != @wfnum;
    my $coll = 'a';
    my @globs = $e->descendants('glob');
    $_ = $_->att('coll') for @globs;
    my %colls = map { $_ => 1 } @globs;
    $coll++ while $colls{$coll};

    foreach (@wfs) {
        if ($_->gi eq 'wf') {
            if (my @id = $_->descendants('id')) {
                $_->set_att(qw/tag un/);
                $_->delete for @id;
            }
            $_->set_gi('cf');
        }
    }
    $lemmas = [ split /\|/, $lemmas ] unless ref $lemmas;
    my %seen;
    $lemmas = join '|', map { $seen{$_}++ ? () : ($_) } @$lemmas;
    my $glob =
	XML::Twig::Elt->new(glob => {
	    tag => 'un',
	    glob => $globtype,
	    lemma => $lemmas,
	    coll => $coll,
            user => $user,
            time => timestamp
	    },
			    '#EMPTY');
    $glob->paste($wfs[0]);
    for (@wfs) {
	my %colls = ( $coll => 1 );
	$colls{$_}++ for split /,/, ($_->att('coll') || '');
        $_->set_att(coll => join(',', sort keys %colls));
    }
    $glob;
}

sub sense_tag_tlc {
    my $self = shift;
    my ($manauto, $wf_num_or_coll, $append, @lemma_and_sk_pairs) = @_;
    my $parent = $self->gi;
    return "Nothing to tag!" if !@lemma_and_sk_pairs && $append;
    return "Odd number of lemma/sks!" unless !(@lemma_and_sk_pairs % 2);

    # What's being tagged?
    my $gi_to_tag = $wf_num_or_coll =~ /^\d+$/ ? "wf" : "cf";
    my $id_attr = $gi_to_tag eq "wf" ? "wf-num" : "coll";

    my $elt = $self->locate_elt_by_id($id_attr, $wf_num_or_coll);
    return "Couldn't find $id_attr=$wf_num_or_coll" unless $elt;

    if ( defined($elt->att("tag")) ) {
        # Don't sense-tag punc or other "ignore" wf elts
        return "Can't sense-tag punctuation!" if defined $elt->att("type") && ($elt->att("type") eq "punc");
        return "Can't sense-tag an ignorable wf!" if $gi_to_tag eq "wf" && ($elt->att("tag") eq "ignore");
    }

    if ( !@lemma_and_sk_pairs || !$append ) {
        # Delete existing tags
        foreach my $id ( $elt->children('id') ) {
            $id->cut;
        }

        if ( !@lemma_and_sk_pairs ) {
            # Are untagging only, so restore wf/cf to pre-tagged
            # state and return.
            
            # Special handling if this is the first word in the
            # immediate constituent
            my $special_handling = $elt->prev_word ? 0 : 1;
            
            if ( my $stop = is_stoplist($elt->text, $special_handling) ) {
                # XXX: just set tag=ignore here, not lemma?
                $elt->set_att(lemma => $stop);
                $elt->set_att(tag => "ignore");
            } else {
                $elt->set_att(tag => "un");
            }
            return (undef, ($parent =~ /^(gloss|examples)$/ ? $elt->index_info_for_elt($parent) : undef));
        }
    }

    my %stags_to_add;

    # Sense tag!
    while (@lemma_and_sk_pairs) {
        my $lemma = shift @lemma_and_sk_pairs;
        my $sk = shift @lemma_and_sk_pairs;

        # Don't tag again if already tagged to this sense!
        my $xp = "id[\@sk=\"$sk\"]";
        next if $elt->children_count($xp);
        
        my $id_num = $elt->next_id("id-num");   
        %stags_to_add = ( id_num => $id_num,
                          sense_tag => $sk,
                          lemma => $lemma,
                          tag_type => $manauto);
                 
        $elt->insert_new_elt('first_child', 'id', {'id-num' => $id_num, 'sk' => $sk, 'lemma' => $lemma}, '#EMPTY');
    }

    $elt->set_att(tag => $manauto);
    # XXX: don't change lemma!
#    $elt->set_lemma_from_sense_tags;
    
    return (undef, ($parent =~ /^(gloss|examples)$/ ? $elt->index_info_for_elt($parent) : undef), \%stags_to_add);
}

sub assign_lemma {
    my $self = shift;
    
    my $gi = $self->gi;
    die "Can't assign_lemma to <$gi> element, sorry!" if $gi !~ /^[cw]f$/;

    if ( $self->children_count('id') ) {
        $self->set_lemma_from_sense_tags;
    } elsif ( $gi eq "cf" && !defined $self->att("glob") ) {
        # Is a non-head cf, delete lemma
        $self->del_att("lemma");
    } else {
        my ($lemma, $form);
        my $type = $self->att("type");
        $type ||= "";
        
        unless ( $form = $self->att("rdf") ) {
            $form = $self->text;
        }
        if ( $type eq "abbr" ) {
            $form =~ s/\'[sd]$//;
            my $rdf;
            my $prevword = $self->prev_word;
            ($lemma, $rdf) = is_abbreviation($form, $prevword);
            $self->set_att(rdf => $rdf) if defined $rdf;
        } else {
            # Remove trailing punctuation (except period) in preparation
            # for lemmatization (but only strip if does not follow
            # final punctuation other than a period!) 
            $form =~ s/([^:;,!\?\ ])[:;,!\?]\ *$/$1/g;
            
            my $special_handling = $self->prev_word ? 0 : 1;
            
            if ( my $stop = is_stoplist($form, $special_handling) ) {
                $lemma = $stop;
                $self->set_att(tag => "ignore");
            } elsif ( $type eq "acronym" ) {
                $lemma = "$form%1";
            } else {
                $lemma = get_lemmas_by_moaning($form);
            }
        }
        $self->set_att(lemma => $lemma);
    }
}

sub prev_word {
    my $self = shift;
    return undef if $self->gi !~ /^[wc]f$/;
    
    my $sib = $self->sibling(-1);
    return undef if !defined $sib || $sib->gi !~ /^[wc]f$/;

    my $type = $sib->att("type");
    return my $prevw = !defined $type || $type ne "punc" ? $sib->text : undef;
}

sub set_lemma_from_sense_tags {
    my $self = shift;
    my @sense_tags = $self->children('id');
    return if !@sense_tags;
    my $lemma;
    foreach my $sense_tag (@sense_tags) {
        next if defined $lemma && $lemma =~ /(^|\|)$sense_tag->att("lemma")($|\|)/;
        $lemma .= "|" if defined $lemma;
        $lemma .= $sense_tag->att("lemma");
    }
    $self->set_att(lemma => $lemma);    
}

sub format_XML_contents {
    my $self = shift;
    my $nosep = shift || 0;

    return $self->text if $self->gi =~ /^[wc]f$/;
    
    my $qu = $self->att('rend') || '';
    my $lq = $qu eq "dq" ? '"' : $qu eq "sq" ? "`" : '';
    my $rq = $qu eq "dq" ? '"' : $qu eq "sq" ? "'" : '';

    my $XML_contents;
    my $sep = $self->gi eq "qf" ? '' : ' ';
    foreach my $elt ( $self->children ) {
        if ( $elt->gi !~ /^[wc]f$/ ) {
            $XML_contents .= $sep . $elt->format_XML_contents;
        } else {
            my $contents = $elt->text;
            $XML_contents = !defined($XML_contents) ? $contents : "$XML_contents$sep$contents";
            my $open_paren = ($elt->is_punc && $elt->text =~ /^([\[({])$/) ? 1 : 0;
            if ( $nosep ) {
                $sep = $open_paren ? '' : ' ';
            } else {
                $sep = $elt->att("sep");
                $sep = " " unless defined $sep or $open_paren;
            }
        }
        $sep = '' if $elt->next_sibling && $elt->next_sibling->is_punc && $elt->next_sibling->text =~ /^[:;,!)?.]/;
    }
    return "$lq$XML_contents$rq";
}

sub index_info_for_elt {
    my $self = shift;
    my ($which_field, $wfi_ref, $stag_ref) = @_;
    $wfi_ref ||= [];
    $stag_ref ||= [];
 
    if ( !defined($which_field) ) {
        if ( $self->parent('gloss') ) {
            $which_field =  "gloss";
        } elsif ( $self->parent('examples') ) {
            $which_field =  "examples";
        } else {
            die "Can only call Tag::XML->index_info_for_elt() method on child of <gloss> or <examples>, sorry!";
        }
    }
    
    # process all <wf/cf>'s with lemmas and id's
    my $elt = $self;
    while ($elt) {
        next if $elt->gi !~ /^[wc]f$/;

        my $lemma = $elt->att("lemma");
        my $wf_num = $elt->att("wf-num");
        my $tagtype = $elt->att("tag");
        next if !defined($lemma) || $tagtype eq "ignore" || !defined($wf_num);

        my @pairs = split /\|/, $lemma;
        my %seen;
        foreach my $wordform_and_pos (@pairs) {
            my ($wordform, $pos) = split /\%/, $wordform_and_pos;
            $wordform = Tag::Functions::_normalize($wordform);
            unless ( defined($seen{$wordform}) ) {
                $seen{$wordform}++;
                push @$wfi_ref, {wf_num => $wf_num, which_field => $which_field, lemma => $wordform, tag_type => $tagtype};
            }
        }
        # Don't put multiple-tagged items into the sense_tags index!
        next if $tagtype eq "un";

        # process <id/>'s for this wf
        foreach my $id ( $self->children('id') ) {
            my $sk = $id->att("sk");
            my $id_num = $id->att("id-num");
            # Every <id/> has exactly one lemma, so no need
            # to a) check if exists, and b) split out multiples
            my $lemma = $id->att("lemma");
            $lemma =~ s/%\d+?$//;
            $lemma = Tag::Functions::_normalize($lemma);
            push @$stag_ref, {id_num => $id_num, lemma => $lemma, tag_type => $tagtype, sense_tag => $sk};
        }
    }
    continue {
        $elt = $elt->next_elt($self);
    }
    return ($wfi_ref, $stag_ref);
}

sub next_id {
    my $self = shift;
    my $attr = shift;
    my $root = $self->root;
    my $elt = $root;
    my $id = 0;
    while ($elt = $elt->next_elt($root) ) {
        if ( my $this_id = $elt->att($attr) ) {
            $id = $id < $this_id ? $this_id : $id;
        }
    }
    return ++$id;
}

sub locate_elt_by_id {
    my $self = shift;
    my ($attr, $idno) = @_;
    my $xp = $attr eq "coll" ? '//*[@coll=~/\b'.$idno.'\b/]' : "//*[\@$attr=\"$idno\"]";
    
    if ( my @results = $self->get_xpath($xp) ) {
        die scalar(@results) . " duplicate $attr(s) found in XML::Twig::Elt::locate_elt_by_id(), $attr=\"$idno\"" if $#results > 0 && $attr ne "coll";
        return $results[0];
    } 
    return undef;
}

my $wcf_lemma_any =   qr/\s\w+\[\|(?:[^\s|]+\|)+\]/;
sub wcf_lemma_cap {  qr/\s(\w+)\[\|(?:[^\s|]+\|)*\Q$_[0]\E\|(?:[^\s|]+\|)*\]/ }
sub wcf_lemma_capi { qr/\s(\w+)\[\|(?:[^\s|]+\|)*\Q$_[0]\E\|(?:[^\s|]+\|)*\]/i }
sub wcf_lemma_repeat {
    local $_ = $_[0];
    return '' unless defined($_);
    $_ = "{$_}" unless $_ eq '*';
    qr/(?:$wcf_lemma_any)${_}?/;
}

our ($_look_inside);
BEGIN { $_look_inside = 'auto'; }
sub mark_parts { shift->initialize_hits(@_, -parts => 1); }
sub initialize_hits {
    my $self = shift;
    my ($search_word, $all_forms_ref, %opts) = @_; # $max_sep) = @_;
    my $justparts = $opts{-parts} ? 1 : 0;
    my $case_sensitive = ($opts{-cs} or $opts{-case_sensitive}) ? 1 : 0;
    my $between = $opts{-between};
    $between = $_words_between unless defined $between;
    my $separation = wcf_lemma_repeat($between >= 0 ? "0,$between" : '*');

    $self->del_att('#hit') for $self->descendants_or_self;
    my @elts = ($self);
    while (grep { ref ne 'ARRAY' } @elts) {
        @elts = map {
            ref eq 'ARRAY' ? $_
                : $_->gi =~ /^(?:[wc]f|glob)$/
                ? ([ $_ ], $_->children('glob'))
                : ([ $_ ], $_->children('#ELT'), [ $_ ]);
        } @elts;
    }

    my $acceptable = $_look_inside eq 'both' ? qr/^(?:man|auto)$/
        : $_look_inside eq 'neither' ? qr/^$/
        : qr/^$_look_inside$/;
    my %bad_col =
        map { ($_->att('coll'), ($_->att('glob') =~ $acceptable ? 0 : 1)) }
    grep { $_->gi eq 'glob' } map { @$_ } @elts;
    @elts = map {
        (grep { $bad_col{$_} } split(/,/, ($_->[0]->att('coll')||'')))
            ? [ ' <blocker>' ]
            : $_;
    } @elts;

    my $stringy = '';
    for (map { @$_ } @elts) {
        if (not ref) {
            $stringy .= $_;
        } elsif ($_->gi eq 'glob' or
            (($_->gi =~ /^[wc]f$/) and
             $_->att('wf-num') and
             ((($_->att('type')||'') ne 'punc')))) {
            $stringy .= 
                ' '.($_->gi eq 'glob' ? $_->att('coll') : $_->att('wf-num'))
                .join('|', '[',
                      (keys %{{( map { $_ => 1; }
                                 $_->lemmas,
                                 ($_->gi ne 'glob' ? $_->text : ()),
                                 map { $_->lemmas }
                                 $_->children('id') )}}),
                      ']');
        } elsif ($_->gi ne 'wf') {
            $stringy .= '<'.$_->gi.'>';
        } elsif ($_->text ne ',') {
            $stringy .= '<punc>';
        }
    }
    warn "\$stringy:\n$stringy\n" if $_mantag_debug;

    my $hits = 0;
    my $regexes = _regexen($search_word, $all_forms_ref, $separation, $case_sensitive);
    for my $regex (@$regexes) {
        warn "=~? $regex\n" if $_mantag_debug;
        while ($stringy =~ /$regex/g) {
            $hits++;
            my @dollars = map { substr($stringy, $-[$_], $+[$_]-$-[$_]) } 1..$#-;
            warn "=~ (@dollars)\n" if $_mantag_debug;
            for my $i (0..$#dollars) {
                my $hit = $dollars[$i];
                ($hit) =
                    ($self->descendants(($hit =~ /\d/
                                         ? '[@wf-num="'
                                         : 'glob[@coll="').$hit.'"]'));
                my $hit_att = ($i or $justparts) ? '#part' : '#hit';
                (my $overt = $hit_att) =~ s/^\#//;
                $hit->set_att($hit_att => 1);
                $hit->set_att($overt   => 1) if $_overt_hits;
            }
        }
    }
    $_->is_hit and $_->del_att('#part'), $_->del_att('part')
        for $self->descendants;
    for ($self->descendants('glob[@#hit]')) {
# grep { $_->gi eq 'glob' and $_->is_hit } $self->descendants) {
        $_->del_att('#hit') for $_->ancestors;
        next unless $_overt_hits;
        $_->del_att('hit')  for $_->ancestors;
    }
    $hits;
}

use Memoize;
sub _regexen {
    my ($word, $all_forms, $separation, $case_sensitive) = @_;
    my @search = map { concatenations(split /[_ ]/, $_) } @$all_forms;
    @search =
        sort { (@$a <=> @$b) or
                   ($b->[0] =~ tr/_/_/  <=> $a->[0] =~ tr/_/_/) or
                   (length($b->[0]) <=> length($a->[0])) } @search;
    [ map {
        join $separation, map {
            $case_sensitive ? wcf_lemma_cap($_) : wcf_lemma_capi($_);
        } @$_;
    } @search ];
}
sub _norm { join $;, @_[0,2,3]; }
memoize('_regexen', NORMALIZER => '_norm');

sub _reset_hits {
    my @e = shift->descendants_or_self;
    for my $att ('#hit', '#part') {
        $_->del_att($att) for @e;
    }
}
sub is_hit { shift->att('#hit') ? 1 : 0 }
sub is_part { shift->att('#part') ? 1 : 0 }

sub sks {
    my $self = shift;
    die "Can't call XML::Twig::Elt::sks on a elt that is not a wf or cf, sorry!" if $self->gi !~ /^[wc]f$/;
    return map { $_->att('sk') } $self->children('id');
}

sub is_tagged {
    my $self = shift;
    my $manorauto = shift;
    $manorauto ||= qr/man|auto/;
    my $tag = $self->att("tag");
    $tag ||= "";
    if ( $self->gi eq "cf" && !defined $self->att("glob") ) {
        # Is a non-head cf of one or more collocations, find head cf(s)
        # and check whether one of them is tagged...
        my @coll_uis = split /,/, $self->att("coll");
        foreach my $coll (@coll_uis) {
            my $head = $self->locate_elt_by_id("coll", $coll);
            return 1 if $head->att("tag") =~ /$manorauto/;
        }
    } else {
        return 1 if $tag =~ /$manorauto/;
    }
    return 0;
}

sub is_punc {
    my $self = shift;
    return 0 if $self->gi !~ /^[wc]f$/;
    return 1 if ($self->att('type') || '') eq "punc";
    return 0;
}

sub is_ignore {
    my $self = shift;
    return 1 if ($self->att('tag') || '') eq "ignore";
    return 1 if $self->parent("*[\@tag=\"ignore\"]");
    return 0;
}
1;

__END__

=head1 NAME

Tag::XML and XML::Twig::Elt -- XML::Twig wrapper and extension functions for WordNet sense-tagging project

=head1 Tag::XML Functions (for Twig root)

=item B<new>

=over 4
 
Creates the XML::Twig object.

=back
 
=head1 XML::Twig::Elt Functions (for elements)

=item B<sense_tag_tlc(>I<$manauto>, I<$wf_num_or_coll>, I<$append>, I<@lemma_and_sk_pairs>B<)>

=over 4
 
Sense tag: The Legend Continues. Sense-tags a cf/wf somewhere in the Twig elt. Passing in an empty list of lemma/sk pairs will result in the cf/wf being un-sense-tagged and returned to its pre-tagged state. Locates the cf/wf by its unique id in $wf_num_or_coll. If not $append mode, or if an empty list is passed in, will first wipe out all existing sense tags. For each lemma/sk passed in, creates a new <id/> child, and sets its attributes sk=, lemma=, and id-num=next id number in sequence. The tag= attribute on the XML::Twig::Elt is set to the value in $manauto, or "un" if untagging. The XML::Twig::Elt must be a gloss, def, ex, or examples element.

Arguments:

=over 4

$manauto is the value to set the tag= attribute to (e.g., "man" or "auto").

$wf_num_or_coll is the unique identifier for the wf/cf being tagged.
 
$append set to any true value will append to existing sense tags, or else wipe them out and re-tag.

@lemma_and_sk_pairs contains the list of $lemma, $sk pairs to tag to. If empty, then all existing sense tags will be deleted, and none added. Each $sk is the WordNet sense key of the word/collocation for the sense tag. Each $lemma is a dictionary (i.e., capitalization intact) form of the word for its corresponding $sk. 

=back

Return values:

=over 4

First value is an error message if an error was raised, or undef if no error. Error msgs are returned if (1) not called on gloss/def/ex/examples, (2) in $append mode but lemma/sk list is empty, (3) an odd number of lemma/sks are passed in, (4) attempting to tag punctuation, (5) the element being tagged is a wf that is "ignorable" (this does not hold for cf's that have tag=ignore, they can be sense-tagged).

The second value is a hash ref of sense tags to add into the sql db (for mantag.pl)

=back
 
Side effects:
                                                
=over 4

The lemma= on the XML::Twig::Elt will be set to the lemma form of the sense tag (or all lemmas concatenated if there is more than one sense tag for the elt). This means that all previous lemma options as returned by moan will be wiped out.

If an empty list of lemma/sks is passed in, the wf/cf will be returned to its pre-tag state.
           
=back 
 
=item B<sense_tag(>I<$sk>, I<$lemma>, I<$tag>B<)>

=over 4
 
Sense tag the Twig element. Creates a new <id/> element, sets attributes 
sk=$sk, lemma=$lemma, and id-num=[next id number in sequence], and
inserts it as the first child of the XML::Twig::Elt. The tag= attribute on the
XML::Twig::Elt is set to the value in $tag. The XML::Twig::Elt can be either a wf or a mwf, but if the $elt is a wf that is a child of a mwf, an error will be returned.
    
Arguments:

=over 4

$sk is the wn sense key of the word/collocation the $elt is being tagged to.

$lemma is the dictionary form of the word. It is not the same as what precedes the % in the sense key, and should be retrieved directly from the wn synset (via the WordNet::Synset->wordnet_form() method).

$tag is the value to set the tag= attribute to (e.g., "man" or "auto").
 
=back

Side effects:
                                                
=over 4

The lemma= on the XML::Twig::Elt will be set to the lemma form of the sense tag (or all lemmas concatenated if there is more than one sense tag for the elt). This means that all previous lemma options as returned by moan will be wiped out.
 
Dies if undefined or null values for $sk, $lemma, and/or $tag are passed as args.
                                               
Returns an error message and does not sense-tag if the XML::Twig::Elt is a child of a mwf (it is thus part of a collocation!) 
                                        
=back 
 
Example:
                                                
    my $tag_error = $elt->sense_tag($sensekey, $lemma, "auto");
    if ( defined($tag_error) ) {
        do some error-handling here (display error, or otherwise
        warn user to unglob first)
    }

where $elt is some <wf> or <mwf> element of the gloss or example.

=back 
 
=item B<un_sense_tag(>I<$sk>B<)>

=over 4

Deletes the sense tag for the XML::Twig::Elt for the sense key passed in via $sk.

Side effects:

=over 4

The tag= attribute on the XML::Twig::Elt is reset to "un" if the un_sense_tag is successful and XML::Twig::Elt has no more sense tags.

Dies if no $sk is passed in, or if more than one sense tag matching $sk is found.
     
Returns an error message if no sense tag matching $sk is found.
           
=back 
 
Example:

     my $tag_error = $elt->un_sense_tag($sk);
     if ( defined($tag_error) ) {
         do some error-handling here...
     }
                                                
=back 8
 
=item B<locate_elt_by_id(>I<$attr>, I<$idno>B<)>

=over 4

Find an element by its id number, which is either the value in wf-num= for <wf>'s or the value in id-num= for <id/>'s. Returns the element within XML::Twig::Elt's branch that matches the values of the $attr and $idno arguments. Dies if more than one element matching the criteria is found. The return value is itself a XML::Twig::Elt object.

Arguments:
             
    $attr is the attribute containing the id. Must be either wf-num or id-num.
    $idno is the id number.
             
=back

=item B<next_id(>I<$attr>B<)>

=over 4

Returns the next id number in sequence. The id attribute is indicated by the $attr argument, which must be either wf-num or id-num.
             
=item B<is_tagged(I<$manorauto>B<)>

=over 4

Returns 1 if XML::Twig::Elt is sense-tagged or is a cf belonging to a collocation that is tagged, 0 otherwise.

Arguments:
             
     $manorauto is an optional argument. Pass in "man" if testing whether Elt
     was tagged manually, and "auto" if it was automatically tagged. If no argument,
     or "" passed in, will test for either.

=back
 
=item B<is_ignore>

=over 4

Returns 1 if XML::Twig::Elt is ignorable, or a child of an ignorable element.

=back

=item B<is_punc>

=over 4

Returns 1 if XML::Twig::Elt is a wf that is punctuation, otherwise returns 0.

=back

=item B<is_hit>

=over 4

Returns zero if the wf is not a hit (or match), and is not part of a collocation that is a hit/match. If the wf is a hit (or part of one), the return value is the numeric value assiged to the hit.

=back

=item B<wfs>

=over 4

Information about wfs in a gloss or example.

Invoked as a method call for a gloss or example "branch", it returns a ref to an array of hashes. Each array element corresponds with a word in the gloss or example. The hash elements are: word (text inside the wf), wf-num (id number of the wf in the wf-num attribute), ignore (1 if wf is "ignorable", 0 otherwise), and is_hit (non-zero if is a hit/match, 0 otherwise).

=back

=item B<sks>

=over 4

All sense keys for the wf or mwf.

Invoked as a method call for a gloss or example "branch", it returns an array ref of sense keys.
 
=back

=item B<initialize_hits(>I<$all_forms_ref>, <$case_sensitive>B<)>

=over 4

Mark all hits/matches in a gloss or example.

Invoked as a method call for a gloss or example "branch", this will find all forms of the search term and mark them so that they can be tested via the is_hit() method. Matching is case-insensitive by default unless $case_sensitive is non-zero.

Arguments:

=over 4

$all_forms_ref is a reference to an array of all forms of the search term. This is what is returned by the call to Tag::Functions::get_all_forms($search_term).

$case_sensitive should be set to 1 if want to override default case-insensitive match.
 
=back

=item B<assign_lemma>

Determines and assigns lemma form(s) for a wf or cf XML::Twig::Elt. It combines the functionality of the Tag::Functions text fns assign_lemma, assign_pseudolemma, and build_mwf_lemma with Tag::XML::Elt->set_lemma_from_sense_tags. It assumes that the data has been previously Tokenized, and thus attributes indicating type="punc|abbr|acronym", tag="ignore", sep=, and rdf= have been set. If the wf/cf is sense-tagged, the Tag::XML::Elt->set_lemma_from_sense_tags method will be called. Otherwise:

If rdf= has been set, the wf contents have been redefined, and this value is used as the word to be lemmatized rather than the tag contents.                                 
If the wf is ignorable, then lemma= will be set to the element contents.

If the wf is an acronym, then lemma= will be automatically set to the contents of PCDATA.

=back
                                     
=item B<set_lemma_from_sense_tags>

Sets the lemma= attribute for a sense-tagged wf/mwf based on the <id/> lemmas. Simple cycles through child <id>'s and concatenates their lemmas, separated by commas.
 
=back
                                     
=item B<format_XML_contents(>I<$nosep>B<)>

Returns the contents of a XML::Twig::Elt that is composed of children wfs, with spacing between words restored based on sep= attributes. Spaces preceding closing puncuation and following an open paren/bracket are automatically closed up.

Pass in $nosep non-null or non-zero if ignore
    
=back
