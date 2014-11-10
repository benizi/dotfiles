package BeniziCrypt;
use IPC::Run 'run';
use Sys::Hostname;
our @ISA = qw/Exporter/;
our @EXPORT = our @EXPORT_OK = qw/&decrypt &encrypt/;

sub _or_self {
	return if ref($_[0]) and __PACKAGE__ eq ref($_[0]);
	unshift @_, __PACKAGE__->new;
}

sub new {
	my $class = shift;
	my $self = bless {@_}, $class;
	$self->_init;
}

sub _init {
	my $self = shift;
	$$self{key} ||= sprintf '%s@%s', scalar(getpwuid($<)), hostname;
#	warn "KEY=$$self{key}\n";
	$self;
}

sub decrypt_file {
	&_or_self;
	my $self = shift;
	my $fn = shift;
	open my $f, '<', $fn or die "<$fn: $!";
	$self->encrypt(do { undef local $/; <$f> });
}

sub encrypt {
	&_or_self;
	my $self = shift;
	my @cmd = ("gpg", "--batch", "-qer", $$self{key});
#	warn "CMD: @cmd\n";
	my ($in, $out, $err);
	$in = shift;
	run \@cmd, \$in, \$out, \$err or die "Encrypt: $?";
	return $out;
}

sub decrypt {
	&_or_self;
	my $self = shift;
	my @cmd = ("gpg", "--batch", "-qdr", $$self{key});
#	warn "CMD: @cmd\n";
	my ($in, $out, $err);
	$in = shift;
#	warn "len(in): ", length($in), "\n";
	run \@cmd, \$in, \$out, \$err or die "Decrypt: $?";
	return $out;
}
1;
