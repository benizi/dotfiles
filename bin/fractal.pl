#!/usr/bin/perl
use strict;
use warnings;
use Tk;
my $mw = MainWindow->new;
$mw->bind('<Key-q>',\&Tk::exit);

my %rules = (map split(/=/), 'L=+RF-LFL-FR+=R=-LF+RFR+FL-',@ARGV);
my $r = $rules{i} || 5;
my $ang = $rules{a} || 90;
$ang *= 6.28318530717959 / 360;
my $TK = $rules{n}?0:1;
my $T = $rules{t}||($TK?513:65);
my @grid = map[map[0,0,0],1..$T],1..$T;
my $c = $TK ? $mw->Canvas(-background=>black=>-width=>$T,-height=>$T)->grid(qw/-sticky news/) : bless{},__PACKAGE__;

$_=0 for my($d,$x,$y,$mx,$my,$Mx,$My,$dx,$dy,$h,$t);
sub np {($x+cos$d),($y+sin$d)}
my @s;
my @p;
my $f = {
	'-' => sub{$d-=$ang},
	'+' => sub{$d+=$ang},
	'[' => sub{@s=(@s,[$x,$y,$d])},
	']' => sub{($x,$y,$d)=@{pop@s}},
	'F' => sub{push@p,$x,$y,np;($x,$y)=np},
	'M' => sub{($x,$y)=np},
};
$$f{r}=sub{my$k=(keys%$f)[rand(0+keys%$f)];$k=~/[\[\]]/ and return;$$f{$k}->()};
$$f{a}=sub{$d+=(-2+4*rand)*$ang};
$$f{$_}||=$$f{F}for qw/A B/;
sub f{($$f{$_}||sub{})->()}

#sub band {$_>255?255:$_<0?0:$_}

sub rgb{my$p=6*shift;map$_*255,map{$_>1?1:0>$_?0:$_}abs(3-$p)-1,map{2-abs($p-$_)}2,4}
my @np;
for my $no (1, 0) {
	$x=$y=$d=0;
	@s=@np=@p=();
	($dx,$dy)=($Mx-$mx,$My-$my);
	($dx>$dy)?($dy=$dx):($dx=$dy);
	my @qs = map [], 1..$r;
	$qs[0] = [split//,$rules{S}||'L'];
#	use Data::Dumper; print Dumper \@qs;
	while (grep @$_, @qs) {
		my $i = $#qs;
		$i-- while $i and not @{$qs[$i]};
#		print "$i has stuff\n";
		my $ar = ($i == $#qs) ? \@np : $qs[$i+1];
		push @$ar, map split(//), map $rules{$_}||$_, shift @{$qs[$i]};
#	use Data::Dumper; print Dumper \@qs;
		for (@np) {
			f;
			if ($no) {
				$mx=$mx<$x?$mx:$x;
				$my=$my<$y?$my:$y;
				$Mx=$Mx>$x?$Mx:$x;
				$My=$My>$y?$My:$y;
				$t+=@p;
				@p=();
			}
			next unless @p;
			$p[$_]=($_ % 2) ? ($p[$_] - $my) / $dy : ($p[$_]-$mx) / $dx for 0..$#p;
			$_*=$T-3 for @p;
			$_+=2 for @p;
my@c;
(@c)=splice@p,0,4 and $c->create(line=>@c,-fill=>sprintf "#%02x%02x%02x", rgb($h-int$h)) and $h+=4/$t while @p;
#(@c)=splice@p,0,4 and $c->create(line=>@c,-fill=>sprintf "#%02x%02x%02x", rgb(rand)) and $h+=1/($t/3.5) while @p;
		}
		@np=();
	}
}
my($v,$o);BEGIN{($v,$o)=("create",2)}
sub create{
	my($V,@u)=(shift,grep!/l/,@_,0)[0..5+1];
	($_=int.1+$_)for(grep{not/["-,]/}@u);
	warn "A @u\n";
	return$V->$v(@u[$o,3,0,1,4,5])if$u[0]>$u[$o];
	warn "B @u\n";
	my@R=map{$u[$_+$o]-$u[$_]}0,1;
	return$V->$v(@u[1,0,3,$o,4],1)if(abs$R[1]>abs$R[0]);
	warn "C @u @R\n";
	$u[5+1]=1;($R[1],$u[-1])=(-$R[1],-1)if(0>$R[1]);
	my($A,$R,$G)=map{$o*$R[1]-$_*$R[0]}0..$o;
	for(;$u[$o]>=$u[0];$u[0]++){
	warn"@u[0,1,2,3,6]\n";
		$a="([0-y][0-y])";
		$grid[$u[$u[5]?1:0]-1][$u[$u[5]?0:1]-1]=[map{hex}$u[4]=~/$a$a$a/];
		if(0>=$R){$R+=$A}else{$R+=$G;$u[1]+=$u[-1]}
	}
	1
}

# sub create {
# 	my$self=shift;
# 	my($line,$x1,$y1,$x2,$y2,$fill,$color,$rev)=@_;
# 	$_=int.1+$_+.1)for$x1,$y1,$x2,$y2;
# 	return $self->create($line,$x2,$y2,$x1,$y1,$fill,$color,$rev) if $x1 > $x2;
# 	my $dx = $x2 - $x1;
# 	my $dy = $y2 - $y1;
# 	return $self->create($line,$y1,$x1,$y2,$x2,$fill,$color,1) if abs$dy>abs$dx;
# #	($dx,$x1,$x2,$dy,$y1,$y2,my($rev)) =
# #		(abs($dx)>abs($dy))
# #		? ($dx,$x1,$x2,$dy,$y1,$y2,0)
# #		: ($dy,$y1,$y2,$dx,$x1,$x2,1);
# 	($dy,my($slope)) = ($dy < 0) ? (-$dy,-1) : ($dy,1);
# 	my ($E,$d,$NE) = map { 2*$dy - $dx*$_ } 0..2;
# 	my $y = $y1;
# 	for (my $x = $x1; $x <= $x2; $x++) {
# 		my ($gx,$gy) = ($rev) ? ($y,$x) : ($x,$y);
# 		plot($gx,$gy,$color);
# 		if ($d <= 0) { $d += $E; }
# 		else { $d += $NE; $y += $slope; }
# 	}
# 	1;
# }
sub plot {
	my ($x,$y,$c) = @_;
	$_--for$x,$y;
	my @COL = (map hex, ($c =~ /(\w\w)/g));
	$grid[$y][$x][$_] = ($grid[$y][$x][$_]*0 + $COL[$_]*2)/2 for 0..$#COL;
}
#$TK or print pack("C*",map@$_,@$_) for @grid;
if($TK){MainLoop}
else{
if($rules{bit}){print map chr, map @$_, map @$_, @grid}
else{print map{(grep$_,@$_)?'*':' '}@$_ and print "\n" for @grid}
}
