export EDITOR=/usr/bin/vim
export WNHOME=/wordnet/wn
export WNSEARCHDIR=/wordnet/wn/dict
export WNSEARCHPATH=$WNSEARCHDIR:/wordnet/wn/2k3/lib/perl/various
export READNULLCMD=less
typeset -U path
pathtest=($HOME/bin /home/benhaskell/bin /home/bhaskell/qmail/bin {/usr{/local,},}/{s,}bin /opt/bin $HOME/bin/dslinux/bin /usr/games/bin /home/bhaskell/wn/bin /var/qmail/bin /usr/kde/4.0/bin /usr/X11R6/bin $path /usr/kde/3.5/bin /people/bhaskell/bin)
path=()
for p in $pathtest ; do [ -d $p ] && path+=($p) done
export PATH
export MANPATH=/usr/share/man:/usr/csl/man:/usr/cogsci/man:/usr/cogsci/X11/man:/usr/dt/man:/usr/openwin/man:/usr/man:/usr/local/man
export PINERC='{dovecot.benizi.com/ssl/user=bhaskell}pinerc'
umask 077
LABPCS=(rapture elation thought dream dictus nym remind wonder felicity bliss serenity)
export LABPCS
export WORDCHARS='*?_-.[]~&;!#$%^(){}<>'
export PERL5LIB=$HOME/Usable
LD_LIBRARY_PATH=$HOME/lib${LD_LIBRARY_PATH+:$LD_LIBRARY_PATH}
typeset -T LD_LIBRARY_PATH ld_library_path
typeset -U ld_library_path
export LD_LIBRARY_PATH
export MOZ5PROF=/home/bhaskell/.mozilla/firefox/qk8tugb3.default
export AXIS2_HOME=/opt/axis2-1.3
export LESS="-R -i -M --shift 5"
export LESSOPEN="|lesspipe.sh %s"
export PAGER=less
export auto_proxy=http://localhost/proxy.pac
export MATLAB=/home/bhaskell/MATLAB/7.4/lib/matlab7
export PYTHONSTARTUP=~/.python/startup
export PYTHONPATH=~/python
THIS_SCRIPT=${(%)${:-%N}}
EXTRAENV=$THIS_SCRIPT
[ -L $EXTRAENV ] && EXTRAENV=$(readlink $EXTRAENV)
[ -e $EXTRAENV ] || EXTRAENV=$THIS_SCRIPT:h/$EXTRAENV
EXTRAENV=$EXTRAENV-
[ -f $EXTRAENV ] && source $EXTRAENV
