#!/usr/bin/env perl
use strict;
use warnings;
use FCGI;
use POSIX qw/setsid dup2/;
use File::Path 'make_path';
use Getopt::Long;
GetOptions(
	'foreground' => \(my $foreground = 0),
	'exec!' => \(my $require_executable = 1),
) or die 'options';

unless ($foreground) { # daemonize
	chdir '/' or die "Can't chdir to /: $!";
	defined(my $pid = fork) or die "Can't fork: $!";
	exit if $pid;
	setsid or die "Can't start a new session: $!";
	umask 0;
}


# test that we can override 'exit'
BEGIN { *CORE::GLOBAL::exit = sub { my $rc = @_ ? shift : 0; die "fakeexit\nrc=$rc\n"; }; }
eval 'exit';
exit if $@ and not $@ =~ /^fakeexit/;

main();

# bhaskell-specific
sub setfacl {
	return if $ENV{LOGNAME} ne 'bhaskell';
	system { 'set-all-facl' } 'set-all-facl', @_;
}

sub main {
	my $sock_dir = '/var/run/nginx';
	my $sock_path = "$sock_dir/perl-fcgi.sock";
	make_path $sock_dir;
	setfacl -u => -w => $sock_dir;

	#my $socket = FCGI::OpenSocket '127.0.0.1:8999', 10; #use IP sockets
	my $socket = FCGI::OpenSocket $sock_path, 10; #use UNIX sockets - user running this script must have w access to the 'nginx' folder!!
	setfacl -w => ':rwX' => $sock_path;

	my $req_params = {};
	my $request = FCGI::Request \*STDIN, \*STDOUT, \*STDERR, $req_params, $socket;
	request_loop($request, $req_params);
	FCGI::CloseSocket $socket;
}

sub request_loop {
	my ($request, $req_params) = @_;
	while ($request->Accept >= 0) {
		#processing any STDIN input from WebServer (for CGI-POST actions)
		my $stdin_passthrough = '';
		my $req_len = 0 + $$req_params{CONTENT_LENGTH};
		if ($$req_params{REQUEST_METHOD} eq 'POST' and $req_len) {
			my $bytes_read = 0;
			while ($bytes_read < $req_len) {
				my $data = '';
				my $bytes = read STDIN, $data, $req_len - $bytes_read;
				last if !$bytes or !defined $bytes;
				$stdin_passthrough .= $data;
				$bytes_read += $bytes;
			}
		}

		#running the cgi app
		if (grep { (!$require_executable or -x) and -s and -r } $$req_params{SCRIPT_FILENAME}) {
			pipe CHILD_RD, PARENT_WR;
			my $pid = open KID_TO_READ, '-|';
			unless (defined $pid) {
				print <<NOOUTPUT;
Content-type: text/plain\r
\r
Error: CGI app returned no output - Executing $$req_params{SCRIPT_FILENAME} failed !
NOOUTPUT
				next;
			}
			if ($pid > 0) {
				close CHILD_RD;
				print PARENT_WR $stdin_passthrough;
				close PARENT_WR;

				print while $_ = <KID_TO_READ>;
				close KID_TO_READ;
				waitpid $pid, 0;
			} else {
				%ENV = (%ENV, %$req_params);
				# cd to the script's local directory
				chdir $1 if $$req_params{SCRIPT_FILENAME} =~ /^(.*)\/[^\/]+$/;
				close PARENT_WR;
				close STDIN;
				dup2 fileno CHILD_RD, 0;
				exec $^X, $$req_params{SCRIPT_FILENAME};
				die 'exec failed';
			}
		} else {
			print <<NOSUCHAPP;
Content-type: text/plain\r
\r
Error: No such CGI app - $$req_params{SCRIPT_FILENAME} may not exist or is not executable by this process.
NOSUCHAPP
		}
	}
}
