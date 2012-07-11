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
pathtest=( ~/brew/bin $^pathtest )
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
eval "$(tied_export CLASSPATH)"
if_exists MOZ5PROF ~/.mozilla/firefox/default
if_exists AXIS2_HOME /opt/axis2-1.3
export auto_proxy=http://localhost/proxy.pac
if_exists MATLAB ~/MATLAB/7.4/lib/matlab7
if_exists PYTHONSTARTUP -f ~$owner/.python/startup
if_exists PYTHONPATH ~$owner/python{,/lib/python*}(N)
if_exists CLOJURE_EXT ~$owner/git/clojure
if_exists M2_HOME ~$owner/maven
if (( $+XAUTHLOCALHOSTNAME )) && (( ! $+XAUTHORITY )) ; then
	export XAUTHORITY=~$owner/.Xauthority
fi
run_local_versions

__clean_ruby_path () {
	local dir
	local -a new_path
	new_path=()
	for dir in $path ; do
		case $dir in
			~$owner/.rbenv*|~$owner/.rvm*) ;;
			*) new_path+=( $dir ) ;;
		esac
	done
	path=( $new_path )
}

setup_ruby () {
	parent_ruby_manager=${PARENT_RUBY_MANAGER:-none}
	(( $+ruby_manager )) || ruby_manager=rbenv

	if [[ -o login ]] || [[ $ruby_manager != $parent_ruby_manager ]] ; then
		__clean_ruby_path
	fi

	# non-interactive setup
	local -a extra_bin
	case $ruby_manager in
		rbenv)
			extra_bin=( ~$owner/.rbenv/bin )
			export RBENV_ROOT=~$owner/.rbenv
			extra_bin=( $RBENV_ROOT/shims $extra_bin )

			rbenv () {
				cmd=$1
				shift
				case "$cmd" in
					shell) eval `rbenv sh-$cmd "$@"` ;;
					*) command rbenv $cmd "$@" ;;
				esac
			}
			;;
		prb)
			extra_bin=( ~g/prb/shims ~g/prb/bin ~$owner/.rbenv/shims )
			[[ -o interactive ]] && . ~$owner/.rbenv/completions/rbenv.zsh
			rbenv rehash 2>/dev/null
			;;
	esac
	(( $+use_prb )) && extra_bin=( ~$owner/prb-bin $extra_bin )

	path=( $extra_bin $path )

	if [[ -o interactive || -o login ]] ; then
		case $ruby_manager in
			rbenv)
				local rbenv_comp=$RBENV_ROOT/libexec/../completions/rbenv.zsh
				[[ -e $rbenv_comp ]] && . $rbenv_comp
				;;
			rvm)
				local rvmsource=~$owner/.rvm/scripts/rvm
				[[ -x $rvmsource ]] && . $rvmsource
				;;
		esac
	fi
	export PARENT_RUBY_MANAGER=$ruby_manager
}

setup_ruby

typeset -A dns_servers
dns_servers=(
	Google-a 8.8.8.8
	Google-b 8.8.4.4
	Dnsadvantage-a 156.154.70.1
	Dnsadvantage-b 156.154.71.1
	OpenDNS-a 208.67.222.222
	OpenDNS-b 208.67.220.220
	Norton-a 198.153.192.1
	Norton-b 198.153.194.1
	Verizon-a 4.2.2.1
	Verizon-b 4.2.2.2
	Verizon-c 4.2.2.3
	Verizon-d 4.2.2.4
	Verizon-e 4.2.2.5
	Verizon-f 4.2.2.6
	Scrubit-a 67.138.54.100
	Scrubit-b 207.225.209.66
)
