#!/usr/bin/perl
use strict;
use warnings;

my (@header, @rest);
while (<>) {
   chomp;
   if (/==UserScript==/../==\/UserScript==/) {
      push @header, $_;
   } else {
      push @rest, $_;
   }
}
my @url;
@header = grep {!(
   /\@require\s+(\S+)/ ? (push @url, $1)
: 0)} @header;
chomp(my @req = map {
   ("/* {{{1 required $_ */",
   readpipe("GET $_"),
   "/* }}}1 required $_ */")
} @url);
print "$_\n" for @header, @req, @rest
__END__
#original:
perl -lnwe '/==UserScript==/../==\/UserScript==/?push@header,$_:push@rest,$_; END { @header = grep {!( /\@require\s+(\S+)/ ? (push @url, $1) : 0 )} @header; chomp(@req = map { ("/* {{{1 required $_ */", readpipe("GET $_"), "/* }}}1 required $_ */") } @url); print for @header,@req,@rest; }' ~/.mozilla/firefox/default/gm_scripts/eosutil/eosutil.user.js > ~/without-require/eosutil.user.js
