# Deal with pathological path on Windows 7 Cygwin
(( $path[(Ie)/bin] )) || export PATH=$PATH:/bin

setopt no_global_rcs

typeset -F 6 SECONDS

if (( $+zsh_start_timing )) ; then
  PS4='$SECONDS %N:%i> '
  exec 3>&2 2>/tmp/startlog.$$
  setopt xtrace prompt_subst
fi

# work around multios bug by not using it
setopt nomultios

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
if (( $+INSOL )) || (( $+INBSD )) || ! is-at-least 4.3.9
then A() { reply=("$(perl -MCwd=realpath -we 'print realpath shift' $REPLY)") }
else A() { reply=( $REPLY(:A) ) }
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

typeset -a -U zsh_dirs
(( $#zsh_dirs == 1 )) && zsh_admin_mode=true
is-at-least 4.3.9 && zshenv=${(%):-"%x"} || zshenv=${(%):-"%N"}
zsh_dirs=( $zshenv(+A:h) ${zshenv:h}/.zsh )
[[ $owner != $USER ]] && zsh_dirs+=( ~$owner/.zsh )
zsh_dirs=( ${^zsh_dirs}{,.local}(N/) )
ZDOTDIR=( $zshenv(+A:h) ) && ZDOTDIR=$ZDOTDIR[1]
if [[ $ZDOTDIR != $zshenv(:h) ]] && [[ $ZDOTDIR = */dotfiles/* ]]
then dotfiles=$ZDOTDIR:h
fi

setup_autoloads() {
  local fn dir
  for dir in ${^zsh_dirs}/{autoload,functions}{,/**/*}(N/) ; do
    fpath+=( $dir )
    autoload -Uz $dir/***(N.:t) >/dev/null
  done
  autoload -Uz .env
}
setup_autoloads

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

setup_term () {
  local dir
  local -a terminfo_dirs
  if (( ! $+commands[infocmp] )) ; then
    return 0
  fi
  term_color_test () { infocmp &> /dev/null }
  if ! term_color_test ; then
    for dir in ${^zsh_dirs}{,(:h)} ~$owner ; do
      terminfo_dirs+=( $dir/{.,}terminfo(N-/) )
    done
    (( $#terminfo_dirs )) && export TERMINFO=$terminfo_dirs[1] 2>/dev/null
  fi
  if ! term_color_test ; then
    ORIGINAL_TERM=$TERM
    for try_term in rxvt-unicode256 xterm-256color xterm ; do
      TERM=$try_term 2>/dev/null
      term_color_test && break
    done
  fi
  term_color_test || TERM=xterm
}
[[ $TERM = 9term ]] || setup_term

if zmodload -F -e zsh/terminfo +p:terminfo && (( $terminfo[colors] > 8 ))
then hi_color=true
else unset hi_color
fi

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
pathtest+=( /usr/bin/vendor_perl /usr/bin/core_perl /usr/bin/site_perl )
pathtest+=( /usr/lib/qt4/bin )
pathtest+=( $HOME/bin/dslinux/bin /usr/games/bin /home/bhaskell/wn/bin /home/bhaskell/qmail/bin /var/qmail/bin /usr/kde/4.0/bin /usr/X11R6/bin )
pathtest+=( $path )
pathtest+=( /usr/local/heroku/bin )
pathtest+=( /people/bhaskell/bin )
pathtest=( ~/.cache/rebar3/bin $pathtest )
pathtest=( ~$owner/maven/bin $^pathtest )
pathtest=( ~/brew/bin $^pathtest )
pathtest=( ~/.cabal/bin $^pathtest )
pathtest=( ~/bin.bin ~/bin.local $^pathtest )
pathtest=( /opt/context/tex/texmf-linux-64/bin $^pathtest )
pathtest=( /opt/fsharp/bin $^pathtest )
pathtest=( {~$owner/{git/,},/opt/}verman/bin $^pathtest )
pathtest=( ~$owner/git/racket/racket/bin $^pathtest )
pathtest+=( ~$owner/android-sdks/platform-tools )
pathtest+=( ~/git/racket/racket/bin )
pathtest=( /opt/djb/bin $^pathtest )
path=( ${^pathtest}(N-/) )
}

# Remove duplicate paths in Arch (where {/bin,/sbin,/usr/sbin} -> /usr/bin)
function () {
local -a arch_same
arch_same=( /{usr/,}{s,}bin(N@) )
(( $#arch_same == 3 )) && path=( ${(@)path:#${(~j,|,)arch_same}} )
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
if_exists perl5lib $dotfiles/perl-lib ~$owner/Usable
eval "$(tied_export LD_LIBRARY_PATH)"
if_exists ld_library_path ~$owner/lib $ld_library_path
eval "$(tied_export CLASSPATH)"
eval "$(tied_export PKG_CONFIG_PATH)"
if_exists MOZ5PROF ~/.mozilla/firefox/default
if_exists AXIS2_HOME /opt/axis2-1.3
unset auto_proxy
if_exists MATLAB ~/MATLAB/7.4/lib/matlab7
if_exists PYTHONSTARTUP -f ~$owner/.python/startup
if_exists PYTHONPATH ~$owner/python{,/lib/python*}(N)
if_exists CLOJURE_EXT ~$owner/git/clojure
if_exists CLOJURESCRIPT_HOME ~$owner/git/clojurescript
if_exists M2_HOME ~$owner/maven
export RUBY_BUILD_CACHE_PATH=/opt/ruby-build-cache
export PIP_DOWNLOAD_CACHE=/opt/pip-download-cache
export BROWSER=sensible-browser

# settings for ssh-proxy
export PROXY_DNS=1
export PROXY_LOCALNET=127.0.0.1:5984/255.0.0.0

if_exists PLAN9 /usr/local/plan9
[[ -e $PLAN9 ]] && path+=( $PLAN9/bin )

if (( $+XAUTHLOCALHOSTNAME )) && (( ! $+XAUTHORITY )) ; then
  export XAUTHORITY=~$owner/.Xauthority
fi

if (( $+commands[verman] )) ; then
  fpath=( $commands[verman]:h:h/zsh $fpath )
  verman_eval() { eval "$(VERMAN_EVAL=1 verman "$@")" }
  _interactive_warn() {
    if [[ -o interactive ]] && [[ -t 1 ]] ; then warn "$@" ; fi
    return 1
  }
  _verman_use() {
    local lang=$1 version=$2
    local var=${lang}_version
    [[ ${(P)var} == $version ]] && return 0
    if (( $+parameters[$var] )) && [[ $parameters[$var] = *-export ]]
    then return $(_interactive_warn "Not overriding exported $var")
    fi
    verman_eval $lang use $version
  }
  _version() {
    local lang=$1 version=$2
    local home=${lang}_home
    _verman_use $lang $version
    [[ -e ${(P)home} ]] && return 0
    local latest=$(verman $lang installed | sed -n '$p')
    if [[ -z $latest ]]
    then return $(_interactive_warn "Not overriding exported $var")
    else _interactive_warn "$lang $latest (wanted $version)"
    fi
    _verman_use $lang $latest
  }

  _version erlang 18.1.5
  _version elixir v1.2.1
  _version node v5.6.0
  _version ruby 2.2.2
  _version rust 1.6.0
  _version go go1.5.2
  _version stack v1.0.2
  _version ocaml 4.02.0
fi

leapd() {
  if [[ -e /opt/leap/usr/bin ]] ; then
    path=( /opt/leap/usr/bin $path )
    ld_library_path=( /opt/leap/LeapSDK/lib/x64 $ld_library_path )
    command leapd "$@"
  fi
}

preso_file=~$owner/presentation
setup_preso() {
  local size=0 force=${1:-false}
  [[ $TERM = *rxvt* ]] || return
  [[ -f $preso_file ]] && size=${preso_large:-40}
  if (( size )) ; then printf '\e]777;font-switch;reset;size=%d\a' $size ; fi
}

if (( $+zsh_start_timing )) ; then
  unsetopt xtrace
  exec 2>&3 3>&-
fi

if (( ! $+MAKEFLAGS )) && (( $+commands[nproc] ))
then
  pmake=-j$(nproc)
  export MAKEFLAGS=$pmake
fi

() { # set up nix
  local nixdir=/opt/nix nixstartup=$nixdir/etc/profile.d/nix.sh
  [[ -e $nixstartup ]] && . $nixstartup
  path=( $nixdir/bin(N) $path )
}

run_local_versions

# Use local SSH agent under Ansible when not forwarding
if (( $+TERM && $+SSH_CONNECTION )) &&
  (( ! $+SSH_AUTH_SOCK && ! $+SSH_AGENT_PID )) &&
  [[ ! -o interactive ]]
then . ${^zsh_dirs}/.zsh_ssh(N) &> /dev/null
fi
