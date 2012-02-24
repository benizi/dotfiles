# Deal with pathological path on Windows 7 Cygwin
(( $path[(Ie)/bin] )) || export PATH=$PATH:/bin
setopt no_global_rcs
# Set up system-specific vars
export ZSH_UNAME=$(uname)
case $ZSH_UNAME in
	CYGWIN_NT-6.1) export INWIN7=true INCYG=true ;;
	*CYGWIN*) export INCYG=true ;;
	*FreeBSD*) export INBSD=true ;;
	*SunOS*) export INSOL=true ;;
	*Darwin*) export INOSX=true ;;
	*) export INLIN=true ;;
esac
# simulate file(:A) as file(+A) if needed
autoload -z is-at-least
if is-at-least 4.3.9 && (( ! $+INSOL )) ; then
	A () { reply=( $REPLY(:A) ) }
else
	A () { reply=("$(perl -MCwd=realpath -we 'print realpath shift' $REPLY)") }
fi
zsh_dirs=(~)
typeset -U zsh_dirs
is-at-least 4.3.9 && zshenv=${(%):-"%x"} || zshenv=${(%):-"%N"}
zsh_dirs+=( $zshenv:h $zshenv(+A:h) )
zsh_dirs=( ${^zsh_dirs}{,.local,-}(N/) )
ZDOTDIR=( $zshenv(+A:h) ) && ZDOTDIR=$ZDOTDIR[1]

() {
local defaultssh=$HOME/.default.ssh
[[ -f $defaultssh ]] && export DEFAULT_SSH="$(<$defaultssh)"
}

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

typeset -U path
function () {
local pathtest user
pathtest=( ${^zsh_dirs}(+A) )
pathtest=( ${^pathtest}/{bin-shared,bin} )
for user in benhaskell bhaskell USC/bhaskell ; do
	pathtest+=( /home/$user/bin-shared /home/$user/bin )
done
pathtest+=( $HOME/python/bin ~/.local/bin $HOME/bin )
(( $+INSOL )) && pathtest+=( /usr/xpg4/bin )
pathtest+=( {/usr{/local,},}/{s,}bin /opt/bin )
pathtest+=( $HOME/bin/dslinux/bin /usr/games/bin /home/bhaskell/wn/bin /home/bhaskell/qmail/bin /var/qmail/bin /usr/kde/4.0/bin /usr/X11R6/bin )
pathtest+=( $path )
pathtest+=( /people/bhaskell/bin )
pathtest=( ~/.gem/ruby/*/bin(N) $^pathtest )
path=( ${^pathtest}(N-/) )
}

export PATH
manpath=( /usr/share/man /usr/csl/man /usr/cogsci/man /usr/cogsci/X11/man /usr/dt/man /usr/openwin/man /usr/man /usr/local/man )
manpath=( ${^manpath}(N-/) )
export PINERC='{dovecot.benizi.com/ssl/user=bhaskell}pinerc'

if_exists () {
	local dir t=-d var=$1
	shift
	for dir ; do
		case $dir in -f|-d) t=$dir ; continue ;; esac
		eval "[[ $t ${(qqq)dir} ]]" && typeset -x $var=$dir
	done
}
(( UID )) && umask 077 || umask 022
export WORDCHARS='*?_-.[]~&;!#$%^(){}<>'
if_exists PERL5LIB ~/Usable
LD_LIBRARY_PATH=$HOME/lib${LD_LIBRARY_PATH+:$LD_LIBRARY_PATH}
typeset -T LD_LIBRARY_PATH ld_library_path
typeset -U ld_library_path
export LD_LIBRARY_PATH
if_exists MOZ5PROF ~/.mozilla/firefox/default
if_exists AXIS2_HOME /opt/axis2-1.3
export auto_proxy=http://localhost/proxy.pac
if_exists MATLAB ~/MATLAB/7.4/lib/matlab7
if_exists PYTHONSTARTUP -f ~/.python/startup
if_exists PYTHONPATH ~/python{,/lib/python*}(N)
if_exists CLOJURE_EXT ~/git/clojure
if_exists M2_HOME ~/maven
run_local_versions
