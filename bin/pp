#!/usr/bin/env ruby
if ENV['PERL']
  require 'tempfile'
  script = Tempfile.new(['pj', '.pl'])
  status = 1
  begin
    script.write(DATA.read)
    script.close
    cmd = ['perl', script.path] + ARGV.dup
    system(*cmd)
    status = $?.exitstatus
  ensure
    script.close
    script.unlink
  end
  exit status
end

require 'rubygems'
require 'pp'
require 'multi_json'

def from_json(input)
  val = MultiJson.decode(input, symbolize_keys: true)
  case val
  when String
    puts val
  else
    p val
  end
end

input = ARGF.read
begin
  from_json(input)
rescue
  input.lines { |line| from_json(line) }
end

__END__
#!/usr/bin/perl
use strict;
use warnings;
use Getopt::Long qw/:config pass_through/;
GetOptions(
	'inplace' => \(my $in_place = 0),
	'bak=s' => \(my $bak = '.bak'),
	'tabs' => \(my $tabs = 0),
	'xml' => \(my $is_xml = 0),
	'json' => \(my $is_json = $0 =~ /j/),
	'linear' => \(my $json_per_line = 0),
) or die 'options';
$^I = $bak if $in_place;
if (!$is_json) {
	my $indent = 0;
	my $space = $tabs ? "\t" : " ";
	my $gi = qr/[\w\-]/;
	while (<>) {
		s/></>\n</gsm;
		my @lines = split /\n/;
		for (@lines) {
			s{^\s+}{};
			s{^<(?![/?!])}{($space x $indent++)."<"}e;
			$indent-- if m{^\s*\S.*</$gi+(?::$gi+)?>};
			$indent-- if m{<[^>]+/>};
			s{^\s*(</$gi+(?::$gi+)?>)}{($space x --$indent).$1}e;
		}
		print "$_\n" for @lines;
	}
} else {
	eval 'use JSON';
	my $json = JSON->new;
	$json->indent(1);
	$json->space_after(1);
	undef local $/ unless $json_per_line;
	while (<>) {
		chomp;
		my $print = $json->encode($json->decode($_));
		$print =~ s{^((?:   )+)}{" " x (length($1)/3)}gem;
		print $print;
	}
}
