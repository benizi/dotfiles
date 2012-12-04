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
pathtest+=( /usr/local/heroku/bin )
pathtest+=( /people/bhaskell/bin )
pathtest=( ~$owner/maven/bin $^pathtest )
pathtest=( ~/brew/bin $^pathtest )
pathtest=( ~/.cabal/bin $^pathtest )
pathtest=( ~/bin.bin ~/bin.local $^pathtest )
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
      ~$owner/.rbenv*|~$owner/.rvm*|~$owner/.rbfu*) ;;
      *) new_path+=( $dir ) ;;
    esac
  done
  path=( $new_path )
  funcs=( ${(k)functions[(I)*rvm*|*rbenv*|*rbfu*]} )
  (( $#funcs )) && unfunction $funcs
}

ruby-manager () {
  local arg save last_manager=~$owner/.ruby-manager
  unset save ruby_manager
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
      extra_bin=( ~$owner/.rbfu/bin )
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
    rbfu)
      eval "$(rbfu --init)"
      unalias rbfu-env
      rbfu-env () { source rbfu "$@" }
      [[ -f ~/.rbfu/default ]] && source rbfu "$(<~/.rbfu/default)" &> /dev/null
      ;;
  esac

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
  (( $+functions[ruby-manager-startup] )) && ruby-manager-startup
  export PARENT_RUBY_MANAGER=$ruby_manager
}

ruby-manager
