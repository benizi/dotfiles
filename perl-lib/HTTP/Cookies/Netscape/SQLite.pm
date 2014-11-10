package HTTP::Cookies::Netscape::SQLite;
use strict;
use warnings;
use vars qw(@ISA $VERSION);
$VERSION = "5.810";
require HTTP::Cookies;
@ISA=qw(HTTP::Cookies);
require DBI;
sub load {
	my ($self, $file) = @_;
	$file ||= $$self{file} || return;
	my $dbh = DBI->connect("dbi:SQLite:dbname=$file", '', '');
	my $now = time() - $HTTP::Cookies::EPOCH_OFFSET;
	my $cookies = $dbh->selectall_arrayref(<<SQL);
select host, path, isSecure, expiry, name, value
from moz_cookies
SQL
	for (@$cookies) {
		my $val = {};
		@$val{qw/host path isSecure expiry name value/} = @$_;
		use Data::Dumper;
		print Dumper \$val;
		my $args = [undef,@$val{qw/name value path host/}, undef,
			0, $$val{isSecure}?1:0, $$val{expiry} - $now, 0];
		$self->set_cookie(@$args);
		use Data::Dumper; print Dumper $args;
	}
	1;
}
1;
