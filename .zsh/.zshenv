# Deal with pathological path on Windows 7 Cygwin
(( $path[(Ie)/bin] )) || export PATH=$PATH:/bin

setopt no_global_rcs

typeset -F 6 SECONDS

if (( $+zsh_start_timing )) ; then
  PS4='$SECONDS %N:%i> '
  exec 3>&2 2>/tmp/startlog.$$
  setopt xtrace prompt_subst
fi

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

setup_term () {
  if (( ! $+commands[infocmp] )) ; then
    return 0
  fi
  term_color_test () { infocmp &> /dev/null }
  if ! term_color_test ; then
    terminfo_dirs=( ${^zsh_dirs}/{.,}terminfo(N/) /usr/share/terminfo(N) )
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

unset hi_color
(( $+terminfo[colors] )) && (( $terminfo[colors] > 8 )) && hi_color=true

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
pathtest=( ~$owner/maven/bin $^pathtest )
pathtest=( ~/brew/bin $^pathtest )
pathtest=( ~/.cabal/bin $^pathtest )
pathtest=( ~/bin.bin ~/bin.local $^pathtest )
pathtest=( ~/context/tex/texmf-linux-64/bin $^pathtest )
pathtest=( /opt/fsharp/bin $^pathtest )
pathtest=( ~$owner/git/verman/bin $^pathtest )
pathtest+=( ~$owner/android-sdks/platform-tools )
pathtest+=( ~/git/racket/racket/bin )
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

run_local_versions

__clean_ruby_path () {
  local dir
  local -a new_path funcs
  new_path=()
  for dir in $path ; do
    case $dir in
      ~$owner/.rbenv*|~$owner/.rvm*|~$owner/.rbfu*|/opt/rbfu/*) ;;
      *) new_path+=( $dir ) ;;
    esac
  done
  path=( $new_path )
  funcs=( ${(k)functions[(I)chruby*|rvm*|rbenv*|rbfu*]} )
  (( $#funcs )) && unfunction $funcs
  unset RUBIES
}

ruby-manager () {
  local arg save last_manager=~$owner/.ruby-manager
  unset save ruby_manager rbfu_dir
  for arg ; do
    case $arg in
      --save) save=true ;;
      *) ruby_manager=$arg ;;
    esac
  done
  parent_ruby_manager=${PARENT_RUBY_MANAGER:-none}
  if (( ! $+ruby_manager )) ; then
    [[ -s $last_manager ]] && ruby_manager=$(<$last_manager) || ruby_manager=rbenv
  fi
  if (( ! UID )) && [[ $ruby_manager = rvm ]] ; then
    warn_rvm_root=true
    ruby_manager=rbenv
  fi

  if (( $+save )) && (( $#ruby_manager )) ; then
    if (( UID )) ; then
      echo $ruby_manager > $last_manager
    else
      echo Not setting manager as root
    fi
  fi

  if [[ -o login ]] || [[ $ruby_manager != $parent_ruby_manager ]] ; then
    __clean_ruby_path
  fi

  # non-interactive setup
  local -a extra_bin
  case $ruby_manager in
    chruby)
      local script=/usr/local/share/chruby/chruby.sh
      if [[ ! -f $script ]] ; then
        unset ruby_manager
        return 1
      fi
      . $script
      ;;
    rbenv)
      local bin=~$owner/.rbenv/bin
      if [[ ! -r $bin ]] ; then
        unset ruby_manager
        return 1
      fi
      extra_bin=( $bin )
      export RBENV_ROOT=$bin:h
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
    rbfu)
      local rbfu
      unset rbfu_dir
      for rbfu in /opt/rbfu ~$ownder/.rbfu ; do
        [[ -d $rbfu ]] || continue
        rbfu_dir=$rbfu
        extra_bin=( $rbfu/bin )
        break
      done
      ;;
    prb)
      extra_bin=( ~g/prb/shims ~g/prb/bin ~$owner/.rbenv/shims )
      [[ -o interactive ]] && . ~$owner/.rbenv/completions/rbenv.zsh
      rbenv rehash 2>/dev/null
      ;;
  esac
  (( $+use_prb )) && extra_bin=( ~$owner/prb-bin $extra_bin )

  path=( $extra_bin $path )

  case $ruby_manager in
    chruby)
      [[ -f ~$owner/.ruby-version ]] && chruby "$(<~$owner/.ruby-version)"
      ;;
    rbfu)
      eval "$(rbfu_dir=$rbfu_dir rbfu --init)"
      unalias rbfu-env
      rbfu-env () { source rbfu "$@" }
      if [[ -f $rbfu_dir/default ]] ; then
        source rbfu @"$(<$rbfu_dir/default)" &> /dev/null
      else
        echo "No default ruby specified in $rbfu_dir/default"
        source rbfu @system
      fi
      ;;
  esac

  if [[ -o interactive || -o login ]] ; then
    case $ruby_manager in
      rbenv)
        local rbenv_comp=$RBENV_ROOT/libexec/../completions/rbenv.zsh
        [[ -e $rbenv_comp ]] && . $rbenv_comp
        ;;
      rvm)
        local rvmsource rvmscripts=~$owner/.rvm/scripts
        for rvmsource in $rvmscripts/{rvm,completion} ; do
          [[ -x $rvmsource ]] && . $rvmsource
        done
        rvm-unfuckup-options() { setopt nokshglob }
        chpwd_functions+=( rvm-unfuckup-options )
        ;;
    esac
  fi
  (( $+functions[ruby-manager-startup] )) && ruby-manager-startup
  export PARENT_RUBY_MANAGER=$ruby_manager
}

ruby-manager

gvm_fixup_path () {
  # remove the "system" go from PATH
  if (( $+gvm_go_name )) ; then
    path=( ${path:#$_my_gobin} )
  else
    path=( ${path:#/opt/gvm*} )
  fi
  typeset -U PATH
}

gvm_use_system () {
  path=( $_my_gobin $path )
  unset gvm_go_name gvm_pkgset_name
  export GOPATH=~$owner/go/external:~$owner/go/code
  unset GOROOT
  gvm_fixup_path
}

go-manager () {
  local gvm=/opt/gvm/scripts/gvm
  _my_gobin=~$owner/hg/go/bin
  if [ -f $gvm ] ; then
    . $gvm
    eval $'gvm () {\n'$functions[gvm]$'\ngvm_fixup_path\n}'
    gvm_fixup_path
  else
    gvm_use_system
  fi
}

go-manager

(( $+commands[nvm] )) && eval "$(nvm default)"

erlang=r15b01
path=( /opt/erlang/$erlang/bin $path )

leapd() {
  if [[ -e /opt/leap/usr/bin ]] ; then
    path=( /opt/leap/usr/bin $path )
    ld_library_path=( /opt/leap/LeapSDK/lib/x64 $ld_library_path )
    command leapd "$@"
  fi
}

preso_file=~/presentation
setup_preso() {
  local size=0 force=${1:-false}
  [[ $TERM = *rxvt* ]] || return

  : ${preso_font="Bitstream Vera Sans Mono"}
  [[ -f $preso_file ]] && size=${preso_large:-18} || size=${preso_normal:-11}

  if (( size )) ; then
    printf '\e]710;xft:%s:size=%d\a' $preso_font $size
    [[ -o interactive ]] && printf 'Set font to %s @ %s\n' $preso_font $size
  fi
}

if (( $+zsh_start_timing )) ; then
  unsetopt xtrace
  exec 2>&3 3>&-
fi

(( $+commands[cpus] )) && pmake=-j$(( $(cpus) + 1 ))
