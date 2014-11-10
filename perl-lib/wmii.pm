package wmii;
use strict;
use warnings;
use IPC::Run 'run';
use Encode 'decode_utf8';
sub r { run [ wmiir => @{shift()} ], @_ }
sub ro { r [@_], '>', \my $o; my $u; eval { $u = decode_utf8 $o }; $@?$o:$u }
sub firstline { (split /\n/, shift)[0] }
sub clients { grep !/sel/, split /\/\n/, ro ls => '/client/' }
sub tags {
	local $_ = @_ ? shift : $_;
	my $tfn = "/client/$_/tags";
	my @tags = sort split /\+/, ro cat => $tfn;
	my $update = @_;
	if ($update) {
		if (ref $_[0]) {
			@tags = sort map @$_, shift;
		} else {
			@tags = sort @tags, @_;
		}
		my %s;
		@tags = grep !$s{$_}++, @tags;
		unshift @tags, '' unless grep !/^\//, @tags;
		r [xwrite => $tfn => join '+', @tags];
	}
	@tags;
}
sub props {
	local $_ = @_ ? shift : $_;
	split /:/, ro(cat => "/client/$_/props"), 3;
}
sub view { firstline ro cat => "/tag/sel/ctl" }
sub sel { firstline ro cat => "/client/sel/ctl" }
package wmii::client;
sub new {
	my $class = shift;
	my $self = bless { id => shift }, $class;
}
sub tags { wmii::tags shift->{id}, @_ }
sub tag { my $t = shift; my $s = shift; (grep { $t eq $_ or eval { $t =~ $_ } } $s->tags) ? 1 : 0 }
sub curr { shift->tag(wmii::view) }
sub tagstring { join '+', shift->tags }
sub props { wmii::props(shift->{id}) }
sub name { (shift->props)[0] }
sub class { (shift->props)[1] }
sub title { (shift->props)[2] }
1;
