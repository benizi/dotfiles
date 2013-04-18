package apachelog;
use base 'Exporter';
our @EXPORT = our @EXPORT_OK = qw/&alog/;
sub alog { apachelog->new(@_ ? shift : $_) }

use Time::Local;

sub new {
   my $self = bless {}, shift;
   @_ == 1 and $self->_parseline(shift);
   $self;
}

our %month_map = qw{
   Jan 1 Feb 2 Mar 3 Apr 4 May 5 Jun 6
   Jul 7 Aug 8 Sep 9 Oct 10 Nov 11 Dec 12
};
sub _parseline {
   my $self = shift;
   local $_ = shift;
   my $orig = $_;
   if (s/^(\d+(?:\.\d+){3})\s//) {
      $$self{ip} = $1;
      $$self{ipv} = 4;
   } elsif (s/^([:\da-f]+)\s//) {
      $$self{ip} = $1;
      $$self{ipv} = 6;
   }
   unless (s/^- //) { s/^(\S+) // and $$self{wtf1} = $1; }
   unless (s/^- //) { s/^(\S+) // and $$self{group} = $1; }
   s{^\[(?<d>\d\d?)/(?<m>\w\w\w)/(?<Y>\d\d\d\d):(?<H>\d\d):(?<M>\d\d):(?<S>\d\d)(?:\s(?<z>(?<zp>[+\-])(?<zh>\d\d)(?<zm>\d\d)))?\] }{} or die "Bad dates: $_\n";
   my %date = map ref()?@$_:$_, %-;
   $date{m} = $month_map{$date{m}};
   $date{m}--;
   my @fields = @date{qw/S M H d m Y/};
   my $t;
   if (!grep !defined, @date{qw/zp zh zm/}) {
      $t = timegm @fields;
      my $mult = $date{zp} eq '-' ? -1 : 1;
      $t -= 60 * $mult * ($date{zh} * 60 + $date{zm});
   } else {
      $t = timelocal @fields;
   }
   $$self{timestamp} = $t;
   s/^"((?:[^\\\"]|\\[\\"])*)" // or warn "Bad request: $_\n";
   $$self{req} = $1;
   @$self{qw/method path httpversion/} = $$self{req} =~ /^([A-Z]+)\s(\S+)\s(\S+)$/;
   unless (s/^- //) { s/^(\d+) // and $$self{status} = $1; $$self{good} = $$self{status} =~ /^[23]/ ? 1 : 0; }
   unless (s/^- //) { s/^(\d+) // and $$self{transferred} = $1; }
   unless (s/^"-" //) { s/^"([^"]*)" // and $$self{refer} = $1; }
   s/"([^"]*)"$// or die "WTF? $orig\n$_\n";
   $$self{useragent} = $1;
   #die "$orig\n$_\n";
   #use Data::Dumper; print Dumper $self;
}

sub _ip2bits {
   shift;
   my @pieces = split /\./, shift;
   join '', map sprintf('%08b', $_), @pieces;
}

{
   my %cached;
   sub _get_mask {
      my ($self, $net, $bits) = @_;
      my $k = join ':', @_;
      if (!$cached{$k}) {
         $bits // (($net, $bits) = split m{/}, $net);
         $net .= '.0' until 3 == $net =~ y/././;
         my $match = $self->_ip2bits($net);
         $cached{$k} = [$match,$bits];
      }
      $self, @{$cached{$k}};
   }
   sub subnet {
      my ($self, $match, $bits) = &_get_mask;
      my $ipbits = $self->_ip2bits($$self{ip});
      # my $match = $self->_ip2bits($net);
      # $match = substr($match, 0, $bits).('.' x (32-$bits));
      # #print "NET=$net\nBIT=$bits\nMAT=$match\n";
      # $ipbits =~ /^$match$/;
      substr($match, 0, $bits) eq substr($ipbits, 0, $bits);
   }
}

sub url { _url_pieces(path=>@_) }
sub refer { _url_pieces(refer=>@_) }
sub _url_pieces {
   my $field = shift;
   my $self = shift;
   local $_ = $$self{$field} // '/';
   s{^(?!http)}{http://www};
   my $p = { whole => $_ };
   s{^(\w+):(?://)?}{} and $$p{scheme} = $1;
   s{^([^/:]+)}{} and $$p{host} = $1;
   $$p{port} = s{^:(\d+)(?=/)}{} ? $1 : 80;
   s{^(/[^?]*)(?=\?|$)}{} and $$p{path} = $1;
   $$p{query} = s/^\?// ? $_ : '';
   my $qs = $$p{query};
   for (split /&/, $qs, -1) {
      next unless length;
      my ($k, $v) = split /=/, $_, 2;
      $v //= \1;
      $$p{q}{$k} = (exists $$p{q}{$k})
         ? [ map { (ref()||'') eq 'ARRAY' ? @$_ : $_ } $$p{q}{$k}, $v ]
         : $v;
   }
   $p;
}

use Digest::MD5 qw/md5_hex md5_base64/;
{
   my %cache;
   sub agent_hash {
      local $_ = shift;
      if (!$cache{$_}) {
         $cache{$_} = substr md5_base64($_), 0, 8;
      }
      $cache{$_};
   }
}

sub uniqagent { &agent->{uniq} }
sub agent { _agent_pieces(useragent=>@_) }
sub _agent_pieces {
   my $field = shift;
   my $self = shift;
   local $_ = $$self{$field} // 'Unknown/1.0';
   my $p = {};
   $$p{uniq} = agent_hash($_);# join '', sort grep /\w/, split //;
   if (/\bMSIE\s(\S+);/) {
   } else {
   }
   $p;
}

1;
