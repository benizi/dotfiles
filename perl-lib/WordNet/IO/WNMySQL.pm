package WordNet::IO::WNMySQL;

use base 'WordNet::IO';
use WordNet qw/:pos/;

our @defaults =
    (
     dbtype   => 'mysql',     # currently only MySQL (the XXX of dbi:XXX:)
     database => 'wn_mysql',
     server   => 'localhost',
     user     => 'anon',
     pass     => 'anon',
     );


sub new {
    my $class = shift;
    my $self = bless { @defaults, @_ }, $class;
    $self->{dbconnstring} =
        join ':', 'dbi', @{$self}{qw/dbtype database server/};
    $self->{dbiconnect} =
        [ $self->{dbconnstring},
          $self->{user},
          $self->{pass},
          { RaiseError => 1 },
          ];
    $self->{DBH} = DBI->connect(@{$self->{dbiconnect}}) or die "Couldn't connect to database";
    $self;
}

=pod

straightforward:

lexical: (synset_id_1, synset_id_2, w_num)
wn_antonym => '!',

wn_attr_adj_noun => '+',

=cut

wn_cause => '>',
wn_class_member => '',
wn_derived => '',
wn_entails => '',
wn_gloss => '',
wn_hypernym => '',
wn_hyponym => '',
wn_mbr_meronym => '',
wn_part_meronym => '',
wn_participle => '',
wn_pertainym => '',
wn_see_also => '',
wn_similar => '',
wn_subst_meronym => '',
wn_synset => '',
wn_verb_frame => '',
wn_verb_group => '',
     );



package WordNet::WNMySQL::Common;

our @ISA = qw/WordNet::Common/;

sub pointers {
    warn "Not implemented yet.\n";
    return ();
}

package WordNet::WNMySQL::Synset;

our @ISA = qw/WordNet::WNMySQL::Common WordNet::Synset/;

package WordNet::Word::WNMySQL; 

our @ISA = qw/WordNet::WNMySQL::Common WordNet::Word/;

1;
__END__
