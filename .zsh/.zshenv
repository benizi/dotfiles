export EDITOR=/usr/bin/vim
export WNHOME=/wordnet/wn
export WNSEARCHDIR=/wordnet/wn/dict
export WNSEARCHPATH=$WNSEARCHDIR:/wordnet/wn/2k3/lib/perl/various
export READNULLCMD=less
typeset -U path
set -A path /home/benhaskell/bin $HOME/bin/dslinux/bin /usr/games/bin $HOME/bin /home/bhaskell/wn/bin /var/qmail/bin $path /usr/kde/3.5/bin /people/bhaskell/bin /sbin /usr/sbin /usr/local/bin /usr/local/sbin /opt/bin
export PATH
export MANPATH=/usr/share/man:/usr/csl/man:/usr/cogsci/man:/usr/cogsci/X11/man:/usr/dt/man:/usr/openwin/man
export PINERC='{dovecot.benizi.com/ssl/user=bhaskell}pinerc'
export CVSROOT=remind.princeton.edu:/usr/local/cvsroot
export CVS_RSH=ssh
export CVS_SERVER=/usr/csl/bin/cvs
umask 077
LABPCS=(rapture elation thought dream dictus nym remind wonder felicity bliss serenity)
export LABPCS
export WORDCHARS='*?_-.[]~&;!#$%^(){}<>'
export LOCALDOMAIN=benizi.com
export PERL5LIB=$HOME/Usable
export MOZ5PROF=/home/bhaskell/.mozilla/firefox/6mbxqig7.default
export AXIS2_HOME=/opt/axis2-1.3
export LESS="-R -i -M --shift 5"
export LESSOPEN="|lesspipe.sh %s"
export PAGER=less
export MATLAB=/home/bhaskell/MATLAB/7.4/lib/matlab7
export PYTHONSTARTUP=~/.python/startup
EXTRAENV=${(%)${:-%N}}
[ -L $EXTRAENV ] && EXTRAENV=$(readlink $EXTRAENV)
[ -f $EXTRAENV- ] && source $EXTRAENV-
