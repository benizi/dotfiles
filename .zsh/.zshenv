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

if (( $+RUBYOPT )) ; then
	warn_rubyopt=$RUBYOPT
	unset RUBYOPT
fi

# find owner of Zsh files (different behavior if root)
if zmodload -F zsh/stat b:zstat 2>/dev/null ; then
	owner="$(zstat -s +uid ${(%):-"%x"})"
else
	echo "Failed to load zstat" >&2
	local -a stat_flags
	(( $+INOSX )) && stat_flags=( -f %Su ) || stat_flags=( -c %U )
	owner="$(stat -L $stat_flags ${(%):-"%x"})"
fi

zsh_dirs=(~$owner ~)
typeset -U zsh_dirs
(( $#zsh_dirs == 1 )) && zsh_admin_mode=true
is-at-least 4.3.9 && zshenv=${(%):-"%x"} || zshenv=${(%):-"%N"}
zsh_dirs+=( $zshenv:h $zshenv(+A:h) )
zsh_dirs=( ${^zsh_dirs}{,.local,-}(N/) )
ZDOTDIR=( $zshenv(+A:h) ) && ZDOTDIR=$ZDOTDIR[1]

defaultssh=~$owner/.default.ssh
[[ -f $defaultssh ]] && export DEFAULT_SSH="$(<$defaultssh)"

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
pathtest+=( {/{usr,opt}{/local,},}/{s,}bin )
pathtest+=( $HOME/bin/dslinux/bin /usr/games/bin /home/bhaskell/wn/bin /home/bhaskell/qmail/bin /var/qmail/bin /usr/kde/4.0/bin /usr/X11R6/bin )
pathtest+=( $path )
pathtest+=( /people/bhaskell/bin )
pathtest=( ~/.rbenv/bin $^pathtest )
pathtest=( ~/prb-bin $^pathtest )
path=( ${^pathtest}(N-/) )
}

export PATH
manpath=( /usr/share/man /usr/csl/man /usr/cogsci/man /usr/cogsci/X11/man /usr/dt/man /usr/openwin/man /usr/man /usr/local/man )
manpath=( ${^manpath}(N-/) )
export PINERC='{dovecot.benizi.com/ssl/user=bhaskell}pinerc'

if_exists () {
	local dir t=-d var=$1
	shift
	[[ $var = [a-z]* ]] && var=$var'[$#'$var'+1]'
	for dir ; do
		case $dir in -f|-d) t=$dir ; continue ;; esac
		test $t $dir && typeset -x $var=$dir
	done
}
tied_export () {
	local name=$1
	print -r - typeset -T $name:u $name:l
	print -r - typeset -x $name:u
	print -r - typeset -U $name:l
}
(( UID )) && umask 077 || umask 022
export WORDCHARS='*?_-.[]~&;!#$%^(){}<>'
eval "$(tied_export PERL5LIB)"
if_exists perl5lib ~$owner/perl-lib ~$owner/Usable
eval "$(tied_export LD_LIBRARY_PATH)"
if_exists ld_library_path ~$owner/lib $ld_library_path
if_exists MOZ5PROF ~/.mozilla/firefox/default
if_exists AXIS2_HOME /opt/axis2-1.3
export auto_proxy=http://localhost/proxy.pac
if_exists MATLAB ~/MATLAB/7.4/lib/matlab7
if_exists PYTHONSTARTUP -f ~$owner/.python/startup
if_exists PYTHONPATH ~$owner/python{,/lib/python*}(N)
if_exists CLOJURE_EXT ~$owner/git/clojure
if_exists M2_HOME ~$owner/maven
run_local_versions

use_prb=false
if (( $+commands[rbenv] )) ; then
	if ! $use_prb ; then
		eval "$(rbenv init -)"
	else
		path=( ~g/prb/shims ~g/prb/bin ~/.rbenv/shims $path )
		[[ -o interactive ]] && . ~/.rbenv/completions/rbenv.zsh
		rbenv rehash 2>/dev/null
		rbenv () {
			cmd=$1
			shift
			case "$cmd" in
				shell) eval `rbenv sh-$cmd "$@"` ;;
				*) command rbenv $cmd "$@" ;;
			esac
		}
	fi
fi

setup_ruby () {
	[[ -o interactive || -o login ]] || return
	local rvmsource=~/.rvm/scripts/rvm do_rvm=false do_rbenv=false
	if (( $+commands[rvm-or-rbenv] )) ; then
		local rvm="$(rvm-or-rbenv -s)"
		[[ $rvm = *rbenv* ]] && do_rbenv=true
		[[ $rvm = *rvm* ]] && do_rvm=true
	else
		[[ -e ~/.rbenv ]] && do_rbenv=true
		[[ -e ~/.rvm ]] && do_rvm=true
	fi
	$do_rbenv && eval "$(rbenv init -)"
	if $do_rvm && [[ -x $rvmsource ]] ; then
		unset RUBYOPT
		. $rvmsource
	fi
}

# setup_ruby
