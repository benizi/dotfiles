#!/usr/bin/perl -l
use strict;
use warnings;
#use open ':utf8', ':std';
use bytes;
use MIME::Base64 qw/encode_base64 decode_base64/;
use Digest::SHA1 qw/sha1_hex sha1_base64/;
use Digest::MD5 qw/md5_hex md5_base64/;
use URI::Escape qw/uri_escape_utf8 uri_unescape/;
use Encode;
use Getopt::Long;
Getopt::Long::Configure(qw/pass_through/);
GetOptions(
	'latin1|lat1|iso-8859-1!' => \(my $do_latin = 0),
	'utf8' => \(my $just_utf8 = 0),
) or die 'options';
$do_latin = 0 if $just_utf8;

while (<>) {
	chomp;
	my @vals;
	if (/^(?:
		[\x00-\x7f]
		|[\xc0-\xdf][\x80-\xbf]
		|[\xe0-\xef][\x80-\xbf]{2}
		|[\xf0-\xf7][\x80-\xbf]{3}
		)*$/x) { # is UTF-8
		@vals = ('UTF-8', decode_utf8($_));
		unshift @vals, 'Latin1', encode('ISO-8859-1', decode_utf8($_)) if $do_latin;
	}
	{
		my @new;
		while (my ($enc, $str) = splice @vals, 0, 2) {
			push @new, $enc, $str;
			next unless $str =~ /%[\da-f]{2}/i;
			push @new, $enc.'d', uri_unescape($str);
		}
		@vals = @new;
	}
	while (my ($enc, $str) = splice @vals, 0, 2) {
		print '';
		print "$enc\t$str";
		print join "\t", "Octets", sprintf '0x%*v02x', ' 0x', $str;
		print join "\t", "octets:d", sprintf '%*v03d', ' ', $str;
		print join "\t", "octets:o", sprintf '\\%*v03o', '\\', $str;
		print join "\t", "chars:d", map sprintf("%03d",ord), grep $_, ($str =~ /(\X)/g);
		if ($str =~ /([A-Za-z0-9\/\+=]{8,})/) {
			print join "\t", "Base64d", $1, decode_base64($1);
		}
		print join "\t", "URI", uri_escape_utf8($str);
		print join "\t", "FullURI", join "", map sprintf("%%%02X",ord), split //, $str;
#		print join "\t", "URI8", uri_escape_utf8($str);
#		next if @vals;
		my $b64 = encode_base64($str);
		$b64 =~ s/\s+//g;
		print join "\t", "Base64e", $b64;
		print join "\t", "SHA-1", sha1_base64($str), sha1_hex($str);
		print join "\t", "MD5", md5_base64($str), md5_hex($str);
	}
}
