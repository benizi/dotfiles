package Linux::Inotify;
use strict;
use warnings;
use Carp;
use POSIX;
use Config;
our $VERSION = '0.01';
our (%mask, %_init);
our $BUFF_SIZE = 65536;
our ($_init, $_add, $_rm);
BEGIN {
	%mask = (
		ACCESS        => 0x00000001,
		MODIFY        => 0x00000002,
		ATTRIB        => 0x00000004,
		CLOSE_WRITE   => 0x00000008,
		CLOSE_NOWRITE => 0x00000010,
		OPEN          => 0x00000020,
		MOVED_FROM    => 0x00000040,
		MOVED_TO      => 0x00000080,
		CREATE        => 0x00000100,
		DELETE        => 0x00000200,
		DELETE_SELF   => 0x00000400,
		UNMOUNT       => 0x00002000,
		Q_OVERFLOW    => 0x00004000,
		IGNORED       => 0x00008000,
		ISDIR         => 0x40000000,
		ONESHOT       => 0x80000000,
		CLOSE         => 0x00000018,
		MOVE          => 0x000000c0,
		ALL_EVENTS    => 0xffffffff
	);
	my %reverse = reverse %mask;
	delete $reverse{$_} for grep sprintf("%b",$_) !~ /^10*$/, keys %reverse;
	sub sprintf_mask {
		my $mask = shift;
		my @bits = grep $mask & $_, keys %reverse;
		join '|', map $reverse{$_}, sort { $b <=> $a } @bits;
	}
	use constant C_UNMOUNT => 211111;
	my $pack = __PACKAGE__;
	for my $c (keys %mask) {
		my $const = <<EVAL;
{ no strict 'refs';
	*{$pack\::$c}->{SCALAR} = $mask{$c};
	*$pack\::$c = sub () { $mask{$c}; };
	\$$pack\::$c = $mask{$c};
}
EVAL
		eval $const;
		$@ and warn "Error in eval($const)\n:$@\n";
#		print "Evaled:\n$const\n$@\n";
	}
	%_init = (
		alpha     => 444,
		arm       => 316,
		x86       => 291,
		ia64      => 1277,
		powerpc   => 275,
		powerpc64 => 275,
		s390      => 284,
		sh        => 290,
		sparc     => 151,
		sparc_64  => 151,
		x86_64    => 253,
	);
}
sub import {
	my ($class, @what) = @_;
	my $caller = caller;
	my %export;
	my $arch = '';
	my @all = qw/_CONSTANTS/;
	my $raw = '';
	@what = grep { not
		exists($mask{$_}) ? ($export{$_}=1) :
		/^:?all$/i ? (@export{@all} = @all) :
		/^:?((?:raw)?)constants?$/i ? ($raw=$1,$export{_CONSTANTS}=1) :
	0 } @what;
	warn "$_ is not exported by ".__PACKAGE__."\n" for @what;
	return 0 if @what;
	$arch ||= $1 if $Config{archname} =~ /([^-]+)-/;
	$arch =~ s/^i[3-6]86$/x86/;
	$_init = $_init{$arch};
	die "unsupported architecture: $arch\n" unless $_init;
	$_add = $_init + 1;
	$_rm = $_init + ($arch !~ /sparc/) ? 2 : 5;
	for my $c ($export{_CONSTANTS}
		? keys(%mask)
		: grep $mask{$_}, keys %export) {
		my $name = "$caller\::".($raw?"":"IN_").$c;
		no strict 'refs';
		eval "*$name = sub () { $mask{$c} };";
		eval "\$$name = $mask{$c};";
	}
#	use Data::Dumper; die Dumper \%export;
}
BEGIN { import(':constants'); }


sub _init { syscall $_init }
sub _add_watch { syscall $_add, @_ }
sub _rm_watch { syscall $_rm, @_ }

sub new {
	my $class = shift;
	my $fd = &_init;
	croak "Linux::Inotify::init() failed: $!" if $fd == -1;
	bless { fd => $fd }, $class;
}

sub add_watch {
	my $self = shift;
	my $watch = Linux::Inotify::Watch->new($self, @_);
	$self->{wd}{$$watch{wd}} = $watch;
	$watch;
}

sub find { my ($self, $key) = @_; $self->{wd}{$key} }

sub close {
	my $self = shift;
	$_->remove for values %{$$self{wd}}; 
	defined(POSIX::close $$self{fd})
		or croak "Linux::Inotify::close() failed: $!";
}


sub read {
	my $self = shift;
	my $bytes = POSIX::read($$self{fd}, my $raw_events, $BUFF_SIZE);
	croak "Linux::Inotify::read: read only $bytes bytes: $!" if $bytes < 16;
	my @ret;
	do {
		my $event = Linux::Inotify::Event->new($self, $raw_events);
		push @ret, $event;
		$raw_events = substr $raw_events, 16 + $$event{len};
	} while(length $raw_events >= 16);
	@ret;
}

package Linux::Inotify::Watch;
use strict;
use warnings;
use Carp;
our @CARP_NOT = 'Linux::Inotify';

sub new {
	my $class = shift;
	my $is_clone = ref $class;
	my ($notifier, $name, $mask) = $is_clone
		? ($$class{notifier}, @_, $$class{mask})
		: (@_);
	$class = $is_clone || $class;
	my $self = {
		notifier => $notifier,
		name => $name,
		mask => $mask,
		valid => 1
	};
	$$self{wd} = Linux::Inotify::_add_watch($$self{notifier}{fd}, $name, $mask);
	croak "Linux::Inotify::Watch::new() failed: $!" if $self->{wd} == -1;
	bless $self, $class;
}
*clone = \&new;

sub invalidate { shift->{valid} = 0 }

sub remove {
	my $self = shift;
	return unless $self->{valid};
	$self->invalidate;
	my $ret = Linux::Inotify::_rm_watch($self->{notifier}->{fd}, $self->{wd});
	croak "Linux::Inotify::Watch::remove(wd=$self->{wd}) failed: $!" if $ret == -1;
}

package Linux::Inotify::Event;
use strict;
use warnings;
BEGIN { Linux::Inotify->import(':rawconstants'); }

sub new {
   my $class = shift;
   my ($notifier, $raw_event) = @_;
   my $self = { notifier => $notifier };
   (my $wd, @$self{qw/mask cookie len/}) = unpack 'iIII', $raw_event;
   $$self{watch} = $notifier->find($wd);
   $$self{name} =
  	 #unpack 'Z*',
	 substr($raw_event, 16, $$self{len});
   $$self{watch}->invalidate if $$self{mask} & DELETE_SELF;
   return bless $self, $class;
}

sub fullname {
   my $self = shift;
   return $$self{watch}{name} . '/' . $$self{name};
}

sub add_watch($) {
   my $self = shift;
   return $$self{watch}->clone($self->fullname);
}

my %reverse;

BEGIN {
   %reverse = (
      0x00000001 => 'access',
      0x00000002 => 'modify',
      0x00000004 => 'attrib',
      0x00000008 => 'close_write',
      0x00000010 => 'close_nowrite',
      0x00000020 => 'open',
      0x00000040 => 'moved_from',
      0x00000080 => 'moved_to',
      0x00000100 => 'create',
      0x00000200 => 'delete',
      0x00000400 => 'delete_self',
      0x00002000 => 'unmount',
      0x00004000 => 'q_overflow',
      0x00008000 => 'ignored',
   );
   my %reverse_copy = %reverse;
   while(my ($key, $value) = each %reverse_copy) {
      $reverse{Linux::Inotify::ISDIR | $key} = "isdir | $value";
   }
}

sub print {
   my $self = shift;
   printf "fd: %d, wd: %d, %21s, cookie: 0x%08x, len: %3d, name: '%s'\n",
      $self->{notifier}->{fd}, ($self->{watch}->{wd}||0), ($reverse{$self->{mask}}||''),
      ($self->{cookie}||0), $self->{len}, $self->fullname()
      ;
}

1;

