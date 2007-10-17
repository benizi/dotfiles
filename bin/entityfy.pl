#!/usr/bin/perl
use strict;
use warnings;
my $verbose = 0;
sub verbose { $verbose and warn @_; 1 }
my @f = @ARGV;
my $in;
my $owls11 = 'http://www.daml.org/services/owl-s/1.1';
my @defaults = (
	'xsd' => qr|/XMLSchema|,
	'profile' => qr|$owls11/Profile\.owl|,
	'service' => qr|$owls11/Service\.owl|,
	'grounding' => qr|$owls11/Grounding\.owl|,
	'process' => qr|$owls11/Process\.owl|,
	'expression' => qr|$owls11/Expression\.owl|,
	'list' => qr|/ObjectList\.owl|,
#	'owl11' => qr|http://www.daml.org/services/owl-s/1.1|,
);
my %defaults = @defaults;
my %order;
$order{$defaults[$_]} = $_+1 for grep !($_%2), 0..$#defaults;
my %entmap;
my %nsmap;
chomp(my @all = <>);
my @pass1 = @all;
while (defined($_ = shift @pass1)) {
	$in = 1 if /<\w+:RDF/;
	if ($in and /xmlns:([\w.]+)="([^"]+)\#"/) {
		my ($ns, $iri) = ($1, $2);
		my $ent = '';
		for my $k (keys %defaults) {
			my $v = $defaults{$k};
			verbose "$iri =~ /$v/?\n";
			if ($iri =~ /$v/) {
				$ent = $k;
				delete $defaults{$k};
				$nsmap{$ns} = $k;
				last;
			}
		}
		verbose "None for $ns -> $iri\n" and next unless $ent;
		$entmap{$iri} = $ent;
	}
	$in = 0 if /<\/\w+:RDF/;
}
my @pass2 = @all;
while (defined($_ = shift @pass2)) {
	while (my ($iri, $ent) = each %entmap) {
		s/\Q$iri\E/&$ent;/g;
	}
	while (my ($ons, $nns) = each %nsmap) {
		s/<(\/?)\Q$ons\E:/<$1$nns:/g;
		s/(?<=xmlns:)\Q$ons\E(?=="[^"]+")/$nns/g;
	}
	print $_, $/;
	if (/<\?xml/ and keys %entmap) {
		print "<!DOCTYPE uridef [\n";
		print "\t<!ENTITY $entmap{$_} \"$_\">\n" for sort {
			($order{$entmap{$a}}||0) <=> ($order{$entmap{$b}}||0)
		} keys %entmap;
		print "]>\n";
	}
}
