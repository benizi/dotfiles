#!/usr/bin/perl
use strict;
use warnings;
my $cache_dir = "$ENV{HOME}/.derequire-cache";

sub cached_name {
   my $url = shift;
   for ($url) {
      y#\x00-\x1f/#-#s;
   }
   $url;
}

sub get {
   my $url = shift;
   my $file = join '/', $cache_dir, cached_name $url;
   unless (-d $cache_dir) {
      system { "mkdir" } "mkdir", "-p", $cache_dir
         and die "Couldn't create cache dir: $cache_dir: $!";
   }
   unless (-f $file and -C $file < 1) {
      my @wget = (wget => -O => $file => $url);
      system { $wget[0] } @wget
         and die "Couldn't run @wget: $!";
   }
   open my $f, '<', $file or die "<$url: $!";
   undef local $/;
   <$f>
}

sub derequire_file {
   local @ARGV = @_;
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
      get($_),
      "/* }}}1 required $_ */")
   } @url);
   print "$_\n" for @header, @req, @rest
}

if (@ARGV == 2) {
     my ($src, $dest) = @ARGV;
     if (-d $src and (!-e $dest or -d $dest)) {
        chdir $src or die "Couldn't chdir($src): $!";
        for my $file (glob '*.user.js') {
           my $mtime = (stat $file)[9];
           my $dfile = "$dest/$file";
           if (-f $dfile and $mtime < (stat $dfile)[9]) {
              print "[$file] Derequired file is newer.\n";
              next;
           }
           print "[$file] Derequiring to [$dfile].\n";
           open my $f, '>', $dfile or die "Couldn't output to $dfile: $!";
           my $stdout = select $f;
           derequire_file $file;
           select $stdout;
        }
        exit;
     }
}

derequire_file $_ for @ARGV;

__END__
#original:
perl -lnwe '/==UserScript==/../==\/UserScript==/?push@header,$_:push@rest,$_; END { @header = grep {!( /\@require\s+(\S+)/ ? (push @url, $1) : 0 )} @header; chomp(@req = map { ("/* {{{1 required $_ */", readpipe("GET $_"), "/* }}}1 required $_ */") } @url); print for @header,@req,@rest; }' ~/.mozilla/firefox/default/gm_scripts/eosutil/eosutil.user.js > ~/without-require/eosutil.user.js
