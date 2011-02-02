# simulates file(:A) as file(+A)
A () { reply=("$(perl -MCwd=realpath -we 'print realpath shift' $REPLY)") }
# Set up system-specific vars
export ZSH_UNAME=$(uname)
case $ZSH_UNAME in
	CYGWIN_NT-6.1) export INWIN7=true INCYG=true ;;
	*CYGWIN*) export INCYG=true ;;
	*FreeBSD*) export INBSD=true ;;
	*SunOS*) export INSOL=true ;;
	*) export INLIN=true ;;
esac
zsh_dirs=(~)
typeset -U zsh_dirs
zshenv=${(%):-"%N"}
zsh_dirs+=( $zshenv:h $zshenv(+A:h) )
zsh_dirs=( ${^zsh_dirs}{,.local,-}(N/) )
ZDOTDIR=( $zshenv(+A:h) ) && ZDOTDIR=$ZDOTDIR[1]

# run versions of the current file that aren't the file itself
function run_local_versions () {
	local file thisfile=${1:-${(%):-"%x"}}
	for file in ${^zsh_dirs}/${thisfile:t}{,-}(.N) ; do
		[ $file = $thisfile(+A) ] && continue
		source $file
	done
}

function term_color_test () { eval '(( $+terminfo[colors] ))' 2>/dev/null }
if ! term_color_test ; then
	terminfo_dirs=( ${^zsh_dirs}/{.,}terminfo(N/) )
	(( $#terminfo_dirs )) && export TERMINFO=$terminfo_dirs[1]
fi
term_color_test || TERM=xterm

export EDITOR=/usr/bin/vim
export READNULLCMD=less
typeset -U path
pathtest=($HOME/python/bin $HOME/bin /home/benhaskell/bin /home/bhaskell/qmail/bin {/usr{/local,},}/{s,}bin /opt/bin $HOME/bin/dslinux/bin /usr/games/bin /home/bhaskell/wn/bin /var/qmail/bin /usr/kde/4.0/bin /usr/X11R6/bin $path /usr/kde/3.5/bin /people/bhaskell/bin)
path=( ${^pathtest}(N-/) )
export PATH
manpath=( /usr/share/man /usr/csl/man /usr/cogsci/man /usr/cogsci/X11/man /usr/dt/man /usr/openwin/man /usr/man /usr/local/man )
manpath=( ${^manpath}(N-/) )
export PINERC='{dovecot.benizi.com/ssl/user=bhaskell}pinerc'
umask 077
export WORDCHARS='*?_-.[]~&;!#$%^(){}<>'
export PERL5LIB=$HOME/Usable
LD_LIBRARY_PATH=$HOME/lib${LD_LIBRARY_PATH+:$LD_LIBRARY_PATH}
typeset -T LD_LIBRARY_PATH ld_library_path
typeset -U ld_library_path
export LD_LIBRARY_PATH
export MOZ5PROF=/home/bhaskell/.mozilla/firefox/qk8tugb3.default
export AXIS2_HOME=/opt/axis2-1.3
export LESS="-R -i -M --shift 5 -F -X -j4"
(( $+commands[lesspipe.sh] )) && export LESSOPEN="|lesspipe.sh %s"
export PAGER=less
export auto_proxy=http://localhost/proxy.pac
export MATLAB=/home/bhaskell/MATLAB/7.4/lib/matlab7
export PYTHONSTARTUP=~/.python/startup
export PYTHONPATH=~/python
run_local_versions
