package Tag::Stoplist;

use 5.006;
use strict;
use warnings;

my @defaults = ( inf_loc  => '/wordnet/wn/2k3/lib/perl/various',
                 wfile => 'waitlist.tok', 
                 sfile => 'stoplist.tok',
                 af_file => 'alloforms.ix',
                 case_sensitive => 1,       # applies to alloforms only
               );

my ($stop_file, $wait_file, $af_file);
my $stoplist_initialized = my $waitlist_initialized = my $af_initialized = 0;

my $case_sensitive = 1;

sub import {
    my @EXPORT = qw(is_stoplist is_waitlist multi_stopwords max_stopforms
                    is_noauto is_alloform);
    my $package = shift;
    my ($caller, $file, $line) = caller();

    use File::Spec;

    my %param = (@defaults, @_);
    $stop_file = File::Spec->catfile($param{inf_loc}, $param{sfile});
    $wait_file = File::Spec->catfile($param{inf_loc}, $param{wfile});
    $af_file = File::Spec->catfile($param{inf_loc}, $param{af_file});

    $case_sensitive = $param{case_sensitive};

    no strict 'refs';
    foreach my $export (@EXPORT) {
        *{"$caller\::$export"} = *{"$package\::$export"};
    }
    _loadStopwords() unless $stoplist_initialized;
    _loadWaitwords() unless $waitlist_initialized;
    _loadAlloforms() unless $af_initialized;
}

my %stoplist = ();
my %waitlist = ();
my %alloforms = ();

sub is_stoplist {
    return _lookup($_[0], ($_[1] ||= 0), \%stoplist);
}

sub is_waitlist {
    return _lookup($_[0], ($_[1] ||= 0), \%waitlist);
}

sub is_noauto {
    my $is_allo = is_alloform($_[0], ($_[1] ||= 0));
    return $is_allo || is_waitlist($_[0], ($_[1] ||= 0));
}

sub is_alloform {
    my ($form, $special_handling) = @_;    
    $form =~ s/[ _-]//go;
    $form = $case_sensitive ? $form : lc $form;
    $form = lcfirst $form if $special_handling && !defined $alloforms{$form};
    return () if !defined $alloforms{$form};
    return split "|", $alloforms{$form};
}

sub max_stopforms {
    return scalar keys %stoplist;
}

sub max_waitforms {
    return scalar keys %waitlist;
}

sub multi_stopwords {
    my $n = shift;
    $n ||= 0;
    return _multi_list ($n, max_stopforms, \%stoplist);
}

sub multi_waitwords {
    my $n = shift;
    $n ||= 0;
    return _multi_list ($n, max_waitforms, \%waitlist);
}

sub _multi_list {
    my ($n, $max, $list) = @_;    
    return () if $n > $max;
    my @mwl = ();
    my $start = !$n ? 2 : $n;
    my $end = !$n ? $max : $n;
    foreach my $m ($start..$end) {
        foreach my $form ( sort { length($b) <=> length($a) or $a cmp $b } keys %{$$list{$m}} ) {
            push @mwl, $form;
        }
    }
    return @mwl;
}

sub _lookup {
    my ($word, $special_handling, $list) = @_;    
    return undef if !defined $word;
    $word =~ s/(\w{2,})'s$/$1/o;
    $word = lcfirst $word if $special_handling && $word =~ /^(?:[A-HJ-Z]|[A-Z][a-z'-])/o && !defined($$list{1}{$word});
    return my $ret = defined($$list{1}{$word}) ? $word : undef;
}

sub _loadStopwords () {
    open (STOP, "<$stop_file") or die "Couldn't open $stop_file!\n";
    while (<STOP>) {
        next if m/^#/;
        s/[\n\r\s]+?$//g;
        next if !/[^\s]/;
        s/_/ /g;
        $stoplist{scalar(my @parts = split ' ', $_)}{$_}++;
        next if !/ /;
        # Treat collocs as one word, as well as multi word form
        $stoplist{1}{$_}++;
    }
    close (STOP);
    $stoplist_initialized = 1;
}

sub _loadWaitwords () {
    open (WAIT, "<$wait_file") or die "Couldn't open $wait_file!\n";
    while (<WAIT>) {
        next if m/^#/;
        s/[\n\r\s]+?$//g;
        next if !/[^\s]/;
        s/,.*?$//;
        s/_/ /g;
        $waitlist{scalar(my @parts = split ' ', $_)}{$_}++;
        next if !/ /;
        # Treat collocs as one word, as well as multi word form
        $waitlist{1}{$_}++;
    }
    close (WAIT);
    $waitlist_initialized = 1;
}

sub _loadAlloforms {
    %alloforms = ();
    if (!-e $af_file) {
        warn "$af_file does not exist\n";
        return;
    }
    open (AF, "<$af_file") or die "Couldn't open $af_file!\n";
    while (<AF>) {
        s/[\n\r\s]+?$//g;
        next if /^#/;
        next unless /^([^\t]+?)\t(.+?)$/;
        $alloforms{$case_sensitive ? $1 : lc $1} = $2;
    }
    close (AF);
    $af_initialized = 1;
}

1;

__END__

=head1 NAME

Tag::Stoplist

=over 4

Functions for dealing with stop and waitlist words

=head1 Functions

=item B<is_Stoplist(>I<$word>, <$special_handling>B<)>

Checks whether passed in $word (single or multi-word form) is a Stoplist item. Returns the Stoplist form of the word if it is, and undef otherwise. Check is case-sensitive, unless $special_handling is set to 1, indicating that the word is the first word in a sentence. In that case, the lcfirst version will be checked, too, providing the word is not "I".

=item B<is_Waitlist(>I<$word>, <$special_handling>B<)>

Checks whether passed in $word (single or multi-word form) is a Waitlist item. Returns the Waitlist form of the word if it is, and undef otherwise. Check is case-sensitive, unless $special_handling is set to 1, indicating that the word is the first word in a sentence. In that case, the lcfirst version will be checked, too, providing the word is not "I".

=item B<is_noauto(>I<$word>, <$special_handling>B<)>

Checks whether passed in $word (single or multi-word form) is a noauto="T" item. Returns 1 if noauto, 0 otherwise. Check will default to case-sensitive, unless either the case_sensitive param is set to false or if $special_handling is set to 1, indicating that the word is the first word in a sentence. In the latter case, the lcfirst version will be checked, too, providing the word is not "I".

=item B<multi_stopwords(>I<$n>B<)>

Returns the list of all n-word stoplist items.

=item B<max_stopforms>

Returns the max # of forms in the list of multi-word stoplist items.

=item B<max_waitforms>

Returns the max # of forms in the list of multi-word waitlist items.

