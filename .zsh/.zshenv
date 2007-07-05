# export DISPLAY=:0
export EDITOR=/usr/bin/vim
export WNHOME=/wordnet/wn
export WNSEARCHDIR=/wordnet/wn/dict
export WNSEARCHPATH=$WNSEARCHDIR:/wordnet/wn/2k3/lib/perl/various
export READNULLCMD=less
typeset -U path
set -A path $HOME/Protege $HOME/bin /home/bhaskell/wn/bin $path /usr/kde/3.5/bin /people/bhaskell/bin /sbin /usr/sbin /usr/local/bin /usr/local/sbin /opt/bin
export MANPATH=/usr/share/man:/usr/csl/man:/usr/cogsci/man:/usr/cogsci/X11/man:/usr/dt/man:/usr/openwin/man
export PINERC='{imap.andrew.cmu.edu/tls/novalidate-cert/user=bhaskell}INBOX.remote_pinerc'
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
export AXIS2_HOME=/home/bhaskell/project/axis2-1.1.1
export LESS="-R -M --shift 5"
export LESSOPEN="|lesspipe.sh %s"
export MATLAB=/home/bhaskell/MATLAB/7.4/lib/matlab7
