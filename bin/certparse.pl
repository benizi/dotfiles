#!/usr/bin/env perl
use 5.020;
use bytes;
use Fcntl qw(SEEK_SET SEEK_CUR);

my %stringtypes = (
  bit => 0x03,
  octet => 0x04,
  utf8 => 0x0c,
  printable => 0x13,
  teletex => 0x14,
  ia5 => 0x16,
);
my %containertypes = (
  sequence => 0x30,
  set => 0x31,
  array => 0xa0,
  array3 => 0xa3, # TODO: what is this?
);
my %basictypes = (
  bool => 0x01,
  int => 0x02,
  null => 0x05,
  oid => 0x06,
  utctime => 0x17,
  generalizedtime => 0x18,
);
my (%id2kind, %id2type);
for (
  ['' => '%s', \%basictypes],
  [string => '%sstring', \%stringtypes],
  [container => '%s', \%containertypes],
) {
  my ($kind, $fmt, $name2id) = @$_;
  while (my ($name, $id) = each %$name2id) {
    my $full = sprintf $fmt, $name;
    $id2kind{$id} = $kind;
    $id2type{$id} = $full;
  }
}

my %oids;
for (
  [qw/at-name 2.5.4.41/],
  [qw/at-surname 2.5.4.4/],
  [qw/at-givenName 2.5.4.42/],
  [qw/at-initials 2.5.4.43/],
  [qw/at-generationQualifier 2.5.4.44/],
  [qw/CN at-x520CommonName 2.5.4.3/],
  [qw/L at-x520LocalityName 2.5.4.7/],
  [qw/ST at-x520StateOrProvinceName 2.5.4.8/],
  [qw/O at-x520OrganizationName 2.5.4.10/],
  [qw/OU at-x520OrganizationalUnitName 2.5.4.11/],
  [qw/at-x520Title 2.5.4.12/],
  [qw/at-x520dnQualifier 2.5.4.46/],
  [qw/C at-x520countryName 2.5.4.6/],
  [qw/at-x520SerialNumber 2.5.4.5/],
  [qw/at-x520Pseudonym 2.5.4.65/],
  [qw/at-domainComponent 0.9.2342.19200300.100.1.25/],
  [qw/at-emailAddress 1.2.840.113549.1.9.1/],
  # missing
  [qw/publicKeyType 1.2.840.10045.2/], # ANSI X9.62
  [qw/ecPublicKey 1.2.840.10045.2.1/], # ANSI X9.62 public key type
  [qw/prime256v1 1.2.840.10045.3.1.7/], # ANSI X9.62 named elliptic curve
  [qw/ecdsaWithSpecified 1.2.840.10045.4.3/], # ANSI X9.62 ECDSA algorithm with Specified
  [qw/ecdsaWithSHA256 1.2.840.10045.4.3.2/], # ANSI X9.62 ECDSA algorithm with SHA256
  [qw/ecdsaWithSHA384 1.2.840.10045.4.3.3/], # ANSI X9.62 ECDSA algorithm with SHA384
  [qw/pkcs-1 1.2.840.113549.1.1/],
  [qw/rsaEncryption 1.2.840.113549.1.1.1/], # PKCS #1
  [qw/sha256WithRSAEncryption 1.2.840.113549.1.1.11/], # PKCS #1
  [qw/sha384WithRSAEncryption 1.2.840.113549.1.1.12/], # PKCS #1
  [qw/sha512WithRSAEncryption 1.2.840.113549.1.1.13/], # PKCS #1
  [qw/sha1WithRSAEncryption 1.2.840.113549.1.1.5/], # PKCS #1
  [qw/sect239k1 1.3.132.0.3/], # SECG (Certicom) named elliptic curve
  [qw/secp384r1 1.3.132.0.34/], # SECG (Certicom) named elliptic curve
  # missing again?
  [qw/nsn 1.2.840.113533.7/],
  [qw/nsn-ce 1.2.840.113533.7.65/],
  [qw/entrustVersInfo 1.2.840.113533.7.65.0/], # Nortel Secure Networks ce
  [qw/enrollCerttypeExtension 1.3.6.1.4.1.311.20.2/], # Microsoft CAPICOM certificate template, V1
  [qw/cAKeyCertIndexPair 1.3.6.1.4.1.311.21.1/], # Microsoft attribute.  Also known as certsrvCaVersion
  [qw/pkix 1.3.6.1.5.5.7/],
  [qw/privateExtension 1.3.6.1.5.5.7.1/], # PKIX
  [qw/authorityInfoAccess 1.3.6.1.5.5.7.1.1/], # PKIX private extension
  [qw/logoType 1.3.6.1.5.5.7.1.12/], # PKIX private extension
  [qw/cert-extension 2.16.840.1.113730.1/], # Netscape
  [qw/netscape-cert-type 2.16.840.1.113730.1.1/], # Netscape certificate extension
  [qw/certExt 2.23.42.7/], # SET
  [qw/hashedRootKey 2.23.42.7.0/], # SET cert extension
  [qw/authorityKeyIdentifier 2.5.29.1/], # X.509 extension.  Deprecated, use 2 5 29 35 instead
  [qw/subjectKeyIdentifier 2.5.29.14/], # X.509 extension
  [qw/keyUsage 2.5.29.15/], # X.509 extension
  [qw/privateKeyUsagePeriod 2.5.29.16/], # X.509 extension
  [qw/subjectAltName 2.5.29.17/], # X.509 extension
  [qw/basicConstraints 2.5.29.19/], # X.509 extension
  [qw/certificatePolicies 2.5.29.3/], # X.509 extension.  Deprecated, use 2 5 29 32 instead
  [qw/nameConstraints 2.5.29.30/], # X.509 extension
  [qw/cRLDistributionPoints 2.5.29.31/], # X.509 extension
  [qw/certificatePolicies 2.5.29.32/], # X.509 extension
  [qw/authorityKeyIdentifier 2.5.29.35/], # X.509 extension
  [qw/extKeyUsage 2.5.29.37/], # X.509 extension
) {
  my ($oid, $syntax, $name) = reverse @$_;
  $oids{$oid} = $name // $syntax;
}

my $der = do { undef local $/; <STDIN> };

{
  my %syms;
  sub metasyn {
    local $_ = shift;
    $syms{$_} //= join ':', sym => 0+keys%syms, $_;
  }
  sub FH {
    return;
    my $fh = pop @_;
    my $eof = (eof($fh) ? " (EOF)" : "");
    warn join(" ", FH => @_)." = ".(metasyn $fh)." @ ".(tell $fh)."$eof\n";
  }
}

{
  sub stringy;
  sub derf;

  my $derf = stringy $der;
  sub derf { $derf }
  sub fhn { my ($n) = @_; @_ > $n ? (stringy $_[$n]) : derf }
  sub fh1 { fhn(1, @_) }
  sub fh2 { fhn(2, @_) }
  sub stringy {
    my $contents = shift;
    return $contents if ref $contents;
    open my $fh, '<', \$contents or die "<string: $!\n";
    $fh;
  }
}

sub get {
  my ($n, $from) = @_;
  my $fh = &fh2;
  return () unless $n;
  local $/ = \$n;
  unpack "C*", scalar <$fh>;
}

sub peek {
  my $fh = &fh2;
  my $off = tell $fh;
  my (@ret) = get @_;
  seek $fh, $off, SEEK_SET;
  @ret;
}

sub octetfy {
  my ($h) = @_;
  for my $bytes (grep defined, $$h{bytes}) {
    $$h{octets} = join ' ', map sprintf("%02x", $_), @$bytes;
    $$h{len} //= @$bytes;
  }
  $h;
}

sub hdr {
  my $fh = &fh1;
  my $off = tell $fh;
  my ($t, $l1) = get 2, $fh;
  my $len = 0;
  my $extended = $l1 >= 0x80;
  my @bytes = $extended ? get $l1-0x80, $fh : ($l1);
  for my $n (@bytes) {
    $len <<= 8;
    $len += $n;
  }
  octetfy {
    reloff => $off,
    typeid => $t,
    kind => $id2kind{$t}//sprintf('unknown:%02x:%1$d', $t),
    type => $id2type{$t}//sprintf('unknown:%02x:%1$d', $t),
    len => $len,
    bytes => [$t, grep($extended, $l1), @bytes],
  }
}

sub value {
  my $fh = &fh1;
  my $h = hdr $fh;
  FH HDR => "[$$h{octets}]" => $fh;
  my @raw = peek $$h{len}, $fh;
  {
    hdr => $h,
    body => octetfy {
      bytes => [@raw],
    },
  }
}

use Data::Dumper;

sub full;
sub expand;
sub addpaths;
sub addoffs;

sub full {
  my $fh = &fh1;
  FH full => $fh;
  return if eof $fh;
  my $f = value $fh;
  seek $fh, $$f{body}{len}, SEEK_CUR;
  if ($$f{hdr}{kind} eq 'container') {
    my @kids;
    my $data = stringy pack 'C*', @{$$f{body}{bytes}};
    FH data => $data;
    until (eof $data) {
      FH data => until => $data;
      last unless my $kid = full $data;
      FH data => testingdefined => $data;
      last unless defined $$kid{hdr}{typeid};
      FH data => pushing => $data;
      push @kids, $kid;
    }
    $$f{kids} = [@kids];
  }
  if ($$f{hdr}{type} eq 'oid') {
    my @bytes = @{$$f{body}{bytes}};
    my $first = shift @bytes;
    my @nodes = (int($first/40), $first % 40);
    while (@bytes) {
      my @node;
      push @node, shift @bytes while @bytes and (!@node or $node[-1] & 0x80);
      my $val = 0;
      for my $node (@node) {
        $val <<= 7;
        $val += $node & 0x7f;
      }
      push @nodes, $val;
    }
    my $oid = join '.', @nodes;
    $$f{oid} = $oid;
    warn "UNKNOWN OID\t$oid\n" unless exists $oids{$oid};
    $$f{name} = $oids{$oid} // sprintf 'unknown:%s', $oid;
  }
  expand $f
}

sub expand {
  addpaths(addoffs(shift))
}

sub addpaths {
  my $root = shift;
  my @q = ([$root,0,'']);
  while (@q) {
    my ($item, $nth, @path) = map @$_, shift @q;
    push @path, sprintf '%s:%d', $$item{hdr}{type}, $nth;
    $$item{path} = join '/', @path;
    #warn "PATH [".Dumper($$item{hdr})."] = [$$item{path}] ".(0+@path)."\n";
    my $n = 0;
    for my $kid (@{$$item{kids} || []}) {
      push @q, [$kid,$n++,@path];
    }
  }
  $root;
}

sub addoffs {
  my $root = shift;
  my @q = ($root,0);
  while (my ($item,$off) = splice @q, 0, 2) {
    my $hdr = $$item{hdr};
    $$hdr{off} = $off + $$hdr{reloff};
    #warn "OFF $$hdr{off} $$item{path}\n";
    $off += $$hdr{len};
    for my $kid (@{$$item{kids} || []}) {
      push @q, $kid, $off;
      $off += $$kid{$_}{len} for qw/hdr body/;
    }
  }
  $root;
}

sub walk {
  use Data::Dumper; print Dumper $_[0]; exit;
  my @q = (shift);
  my @ids;
  while (my $i = shift @q) {
    push @q, @{$$i{kids}//[]};

    # Finding OIDs
    next unless $$i{hdr}{type} eq 'oid';
    next unless $$i{oid} =~ m{^\Q2.5.4.\E};
    #next unless $$i{path} =~ m{^\Q/sequence[0]/sequence[0]/sequence[3]/\E};
    #next unless $$i{name} eq 'CN';
    #use Data::Dumper; print Dumper $i;
    #say join "\t", @$i{qw/oid name path/};
    push @ids, $i;

    # finding structure
    #next unless $$i{path} =~ m{^\Q/sequence:0/sequence:0\E/[^/]+$};
    #say $$i{path};
  }
}

my @path = ([]);
#use Data::Dumper; print Dumper \%id2kind, \%id2type;
#use Data::Dumper; print Dumper full;
walk full;

__END__
$container{$_} = 1 for map hex, qw(30 a0);
my @want = map eval, @ARGV; warn "WANT[@want]\n"; while (@want) { my $go = $want[0]; my ($t,$l)=hdr; warn "GOT[t=$t l=$l]\n"; if ($t == $go) { shift @want } else { get $l } }' 0x30 LS
# original:
openssl x509 -outform der < /tmp/cert.0103 |
perl -Mbytes -we '
sub get { my $n=shift; local $/=\$n; unpack "C*", scalar <STDIN> }
sub hdr { my $t = get 1; my $l1 = get 1; my @l = $l1<=0x80 ? ($l1) : get $l1-0x80; my $len = 0; for my $n (@l) { $len <<= 8; $len += $n; } ($t, $len, @l) }
my ($t,$l)=hdr;
warn "READ[t=$t l=$l]\n"
'
