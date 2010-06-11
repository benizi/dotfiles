#!/usr/bin/perl
use strict;
use warnings;
my $need_hunk = 0;
my (@A, @B);
my @ind;
while (<>) {
   my $orig = $_;
   if ($need_hunk) {
      my @in = (
         s/^ // ? (\@A,\@B) :
         s/^-// ? (\@A) :
         s/^\+// ? (\@B) :
         die "Expecting line of hunk, but got: $orig\n"
      );
      my $nolead = $_;
      push @{$$_[-1]}, $nolead for @in;
      $need_hunk-- for @in;
   } elsif (/^\@\@ -(\d+),(\d+) \+(\d+),(\d+) \@\@/) {
      $need_hunk = $2 + $4;
      if (@ind) {
         @ind == 2 or die "Didn't find proper indexes around line $.\n";
         push @$_, [{ filename => shift @ind }] for \@A, \@B;
      }
      push @$_, [] for \@A, \@B;
   } else {
      if (/^diff\s(?:-\S+\s)*(\S+) (\S+)/) {
         push @ind, $1, $2;
      } elsif (/^--- (\S+)/) {
         @ind >= 2 or unshift @ind, $1;
      } elsif (/^\+\+\+ (\S+)/) {
         @ind >= 2 or push @ind, $1;
      } else {
         # header etc.
         push @$_, [{ header => $orig }] for \@A, \@B;
      }
   }
}
use ColorDiff;
#ColorDiff::Format('html');
ColorDiff::Split('punc');
#ColorDiff::Split('words');
for (0..$#A) {
   my ($l, $r) = ($A[$_],$B[$_]);
   if (@$l and ref($$l[0])) {
      ($l, $r) = map shift(@$_), $l, $r;
      if ($$l{filename}) {
         print ColorDiff::color_diff(
            ["--- $$l{filename}"],
            ["+++ $$r{filename}",''],
         );
      } elsif ($$l{header}) {
         print "$$l{header}";
      } else {
         die "Unknown special field: @{[keys %$l]}\n";
      }
   } else {
      print ColorDiff::color_diff($l, $r);
      print "\n";
   }
}
