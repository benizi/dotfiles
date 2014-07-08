package y::Birthday;
use base 'Exporter';
sub sym { my $s = \%::; $s = $$s{$_.'::'} for split /::/, __PACKAGE__; $s }
my $A; BEGIN { $A = {%{(sym)}} }
our $birthday_epoch = our $birthday = 349230240; # 6:24PM CST
our $dawni_epoch = our $dawni_birthday = 361731600; # noon CDT
BEGIN { our @EXPORT = our @EXPORT_OK = map "*$_", grep !$$A{$_}, keys %{(sym)} }
1;
