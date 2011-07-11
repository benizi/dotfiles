#!/usr/bin/perl
use strict;
use warnings;
use Getopt::Long;
GetOptions(
	'files' => \(my $create_files = 0),
	'diffcmd=s' => \(my $diff_cmd = 'kdiff3'),
	'mac' => \(my $handle_bare_cr = 0),
	'keep' => \(my $keep = 0),
) or die 'options';
my $need_hunk = 0;
my @AB = \my (@A, @B);
my @ind;
my @in;
sub dollar { my ($str, $n) = @_; substr $str, $-[$n], $+[$n]-$-[$n] }
while (<>) {
	my $orig = $_;
	if ($need_hunk and $handle_bare_cr and /\r(?!\n)/) {
		my ($type) = /^([+\-\ ])/;
		s/\r(?!\n)/\n/g;
	}
	if (/^\\ No newline at end of file/ and @in == 1) {
		my $lines = $in[0][-1];
		$lines // die "No newline at end of what file?\n";
		$$lines[-1] =~ s/(?=[\r\n]*$)/<no newline>/;
	} elsif (/^Files (.*?) and (.*?) differ/) {
		push @{$AB[$_]},
			[{ filename => dollar $orig, $_+1 }],
			[ 'Binary files differed' ]
			for 0..$#AB;
	} elsif (/^Only in (.*?): (.*?)$/) {
		push @{$AB[$_]},
			[{ filename => $1.'/'.$2.'('.$_.')' }],
			[ 'Path only in one or the other' ]
			for 0..$#AB
	} elsif ($need_hunk) {
		@in = (
			s/^ // ? (\@A,\@B) :
			s/^-// ? (\@A) :
			s/^\+// ? (\@B) :
			die "Expecting line of hunk, but got: $orig\n"
		);
		my $nolead = $_;
		push @{$$_[-1]}, $nolead for @in;
		$need_hunk-- for @in;
	} elsif (/^\@\@ -(\d+)(?:,(\d+))? \+(\d+)(?:,(\d+))? \@\@/) {
		$need_hunk = ($2//1) + ($4//1);
		if (@ind) {
			@ind == 2 or die "Didn't find proper indexes around line $.\n";
			push @$_, [{ filename => shift @ind }] for @AB;
		}
		push @$_, [] for @AB;
	} else {
		# if (/^diff\s(?:-\S+\s)*(\S+) (\S+)/) {
		# push @ind, $1, $2;
		# } els
		if (/^--- (\S+)/) {
			@ind >= 2 or unshift @ind, $1;
		} elsif (/^\+\+\+ (\S+)/) {
			@ind >= 2 or push @ind, $1;
		} else {
			# header etc.
			push @$_, [{ header => $orig }] for \@A, \@B;
		}
	}
}
for my $arr (@AB) {
	for my $i (reverse 0..$#$arr-1) {
		my ($this, $next) = @$arr[$i,$i+1];
		next if grep !@$_, $this, $next;
		next if grep !ref($$_[0]), $this, $next;
		next if grep !exists($$_{header}), map $$_[0], $this, $next;
		$$this[0]{header} .= $$next[0]{header};
		splice @$arr, $i+1, 1;
	}
}
use ColorDiff;
#ColorDiff::Format('html');
ColorDiff::Split('punc');
#ColorDiff::Split('words');
my (@files, @filenames);
if ($create_files) {
	eval 'use File::Temp; 1' or die "$@";
	for (1,2) {
		my ($fh, $filename) = &File::Temp::tempfile;
		push @files, $fh;
		push @filenames, $filename;
	}
}
for (0..$#A) {
	my ($l, $r) = ($A[$_],$B[$_]);
	if (@$l and ref($$l[0])) {
		my @lr = ($l, $r) = map shift(@$_), $l, $r;
		if ($$l{filename}) {
			if (@files) {
				for (0..$#lr) {
					my $fh = $files[$_];
					print $fh "====> $lr[$_]{filename} <====\n";
				}
			} else {
				print ColorDiff::color_diff(
					["--- $$l{filename}"],
					["+++ $$r{filename}",''],
				);
			}
		} elsif ($$l{header}) {
			my @toprint = @files ? @files : (\*STDOUT);
			my @lines = map [$$l{header}], @toprint;
			if(1){#if (@files) {
				unshift @{$lines[$_]},
					map { ($_ x 72)."\n" }
					'=', [qw/< >/]->[-$_], '='
					for 0..$#lines;
			}
			print $_ @{shift @lines} for @toprint;
			#print $_ "HEADER\n$$l{header}" for (@files) ? @files : (\*STDOUT);
		} else {
			die "Unknown special field: @{[keys %$l]}\n";
		}
	} else {
		if (@files) {
			my @line_arrays = ($l, $r);
			for my $i (0..$#files) {
				my $fh = $files[$i];
				my $lines = $line_arrays[$i];
				print $fh map { "\n" } 1..5;
				print $fh @$lines;
			}
		} else {
			print ColorDiff::color_diff($l, $r);
			print "\n";
		}
	}
}
sub close_files { close $_ for @files; @files = () }
sub unlink_files { $keep or unlink $_ for @filenames; @filenames = () }
close_files;
if ($create_files) {
	my @cmd;
	if ($diff_cmd =~ /%s/) {
		for (split ' ', $diff_cmd) {
			push @cmd, /%s/ ? shift @filenames : $_;
		}
	} else {
		@cmd = ($diff_cmd, @filenames);
	}
	system { $cmd[0] } @cmd;
}
unlink_files;
END { close_files; unlink_files; }
