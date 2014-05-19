#!/usr/bin/perl
use strict;
use warnings;
use feature ':5.10';
use POSIX 'strftime';
use M;
$|=1;
my $phase = 0;
my $host;
my $clen;
my $responses = 0;
while (<>) {
	chomp;
	warn "LINE:{$_}\n";
	warn "PASSWORD{", base64_decode($1), "}\n" if /Auth.*:\sBasic\s(\S+)/;
	tr/\r//d;
	$host = "Host: $1" if /Host:\s(\S+)/;
	$clen = $1 if /Content-Length: (\d+)/;
	if (!/\S/) {
		if ($clen) {
			local $/ = \$clen;
			my $got = <>;
			warn "GOT{$got}\n";
			my $date = strftime '%a, %d %b %Y %T %Z', localtime;
			my $time = $responses++;
			my $response = $time ? <<'FIND' : <<'RESPONSE';
<?xml version="1.0" encoding="utf-8"?>
<D:multistatus xmlns:D="DAV:">
<D:response xmlns:S="http://subversion.tigris.org/xmlns/svn/" xmlns:C="http://subversion.tigris.org/xmlns/custom/" xmlns:V="http://subversion.tigris.org/xmlns/dav/" xmlns:lp1="DAV:" xmlns:lp2="http://subversion.tigris.org/xmlns/dav/" xmlns:lp3="http://apache.org/dav/props/">
<D:href>/testing/</D:href>
<D:propstat>
<D:prop>
<lp1:resourcetype><D:collection/></lp1:resourcetype>
<lp1:getcontenttype>text/html; charset=UTF-8</lp1:getcontenttype>
<lp1:getetag>W/"2//"</lp1:getetag>
<lp1:creationdate>2008-03-31T19:01:36.255570Z</lp1:creationdate>
<lp1:getlastmodified>Mon, 31 Mar 2008 19:01:36 GMT</lp1:getlastmodified>
<lp1:checked-in><D:href>/testing/!svn/ver/2/</D:href></lp1:checked-in>
<lp1:version-controlled-configuration><D:href>/testing/!svn/vcc/default</D:href></lp1:version-controlled-configuration>
<lp1:version-name>2</lp1:version-name>
<lp1:creator-displayname>MEDRESPOND\bhaskell</lp1:creator-displayname>
<lp2:baseline-relative-path/>
<lp2:repository-uuid>145d8f68-c37f-6d4b-8e3f-0180fe2a64fb</lp2:repository-uuid>
<D:lockdiscovery/>
</D:prop>
<D:status>HTTP/1.1 200 OK</D:status>
</D:propstat>
</D:response>
</D:multistatus>
FIND
<!DOCTYPE HTML PUBLIC "-//IETF//DTD HTML 2.0//EN">
<html><head>
<title>401 Authorization Required</title>
</head><body>
<h1>Authorization Required</h1>
<p>This server could not verify that you
are authorized to access the document
requested.  Either you supplied the wrong
credentials (e.g., bad password), or your
browser doesn't understand how to supply
the credentials required.</p>
<hr>
<address>Apache/2.0.63 (Win32) DAV/2 mod_auth_sspi/1.0.4 mod_ssl/2.0.63 OpenSSL/0.9.7m SVN/1.3.2 Server at svn.medrespond.local Port 786</address>
</body></html>
RESPONSE
			my $rlen = ($response =~ tr/\n/\n/) + length $response;
			my $head_body = $time ? <<GETIT : <<LOGIN;
HTTP/1.1 207 Multi-Status
Date: $date
Server: Apache/2.0.63 (Win32) DAV/2 mod_auth_sspi/1.0.4 mod_ssl/2.0.63 OpenSSL/0.9.7m SVN/1.3.2
Content-Length: $rlen
Content-Type: text/xml; charset="utf-8"

$response
GETIT
HTTP/1.1 401 Authorization Required
Date: $date
Server: Apache/2.0.63 (Win32) DAV/2 mod_auth_sspi/1.0.4 mod_ssl/2.0.63 OpenSSL/0.9.7m SVN/1.3.2
WWW-Authenticate: NTLM
WWW-Authenticate: Basic realm="MedRespond Subversion"
Content-Length: $rlen
Content-Type: text/html; charset=iso-8859-1

$response
LOGIN
			$head_body =~ s/\n/\r\n/gsm;
			print $head_body;
			warn "WROTE{\n==$head_body==}\n";
		}
	}
}
