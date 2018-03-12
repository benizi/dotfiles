bindkey -e
bindkey "^[" vi-cmd-mode

bindkey "^T" expand-word
(( $+widgets[history-incremental-pattern-search-backward] )) \
&& bindkey "^R" history-incremental-pattern-search-backward
setopt nohup no_notify no_check_jobs
setopt auto_cd auto_pushd no_auto_menu
setopt extended_glob glob_dots hist_subst_pattern 2>/dev/null
setopt pushd_silent
setopt no_recexact no_rcquotes
setopt magic_equal_subst
setopt csh_junkie_history
export MAILCHECK=0
autoload -U compinit bashcompinit
autoload -Uz age after before
zmodload zsh/mathfunc 2>/dev/null
fpath=( ${^zsh_dirs}/Completion(N/) ~$owner/git/zsh/Completion/**/*(N/) $fpath $^zsh_dirs/autoload(N/) )
debug=( ~$owner/.zcompdump.debug{,ging}(N) )
(( $#debug )) && rm -f ~/.zcompdump
compinitargs=( -d ~/.zcompdump )
() {
  local -a notmine
  notmine=( $fpath(N^U) )
  (( $+INCYG )) || (( $#notmine )) && compinitargs+=( -u )
}
compinit $compinitargs
bashcompinit
[ -n "$INCYG" -a -n "$INWIN7" ] && export CYGWIN=nontsec

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
[[ $TERM = (9term) ]] || setup_term

zmodload -F -e zsh/terminfo +p:terminfo
hi_color() { (( $terminfo[colors] > 8 )) }

export LESS="-R -i -M --shift 5 -F -X -j4"
(( $+commands[lesspipe.sh] )) && export LESSOPEN="|lesspipe.sh %s"
export PAGER=less
export EDITOR=vim
export READNULLCMD=$PAGER

for dir in $zsh_dirs ; do
  setopt nullglob
  pushd $dir 2> /dev/null || continue
  files=(.zshrc-)
  if [ -f .ZSHFILES ] ; then
    files=($files `cat .ZSHFILES`)
  else
    files=($files *zsh_*~*.swp~*.zsh_history~*.orig~*.*.*)
  fi
  for file in $files ; [ -r $file ] && source $file
  popd
  setopt nonullglob
done

() {
  local -a files
  files=( ${(0)^:-"$(reminder-files)"}(N) )
  (( ! $#files )) || headline $files
}

run_local_versions ${(%):-"%x"}
#screen -ls 2>/dev/null | grep -q Detached && exec screen -rr
#[ -n "$SSH_CLIENT" ] && [ "$TERM" != "screen" ] && exec screen -RR -- zsh -l

typeset -a command_not_found_handlers
command_not_found_handler () {
  local fn
  for fn in $command_not_found_handlers ; do
    $fn "$@" && return 0
  done
  return 1
}

all_git_aliases () {
  git config -l | awk -F'[.=]' '/^alias\./ { print $2 }'
}

valid_git_alias () {
  git config -l | grep -qF "alias.$1="
}

all_git_commands () {
  git help --all \
    | awk '/---|available git commands/ {ok=1; OFS="\n"; ORS=""} /^ / {NF=NF+1; if (ok) print $0}'
  print -l ${${(k)commands[(I)git-*]}#git-}
}

valid_git_command () {
  all_git_commands | grep -qF "$1" || (( $+commands[git-$1] ))
}

auto_git_alias () {
  [[ $1 = g* ]] || return 1
  local al=${1#g}
  shift
  valid_git_alias $al || valid_git_command $al || return 1
  git $al "$@"
  return 0
}
(( $+commands[git] )) && command_not_found_handlers+=( auto_git_alias )

TRAPZERR () {
  [[ $zsh_eval_context[1] == toplevel ]] || return 0
  local dir= choice banner
  unset choice
  local -a dirs
  set -- ${=__last_command}
  (( $# == 1 )) || return 0
  [[ $1 == */* ]] || return 0
  [[ $1 == "<"* ]] && return 0
  dir=${~1}
  dirs=( ${~:-$dir*}(-/N) )
  if [[ ! -e $dir ]] && (( $#dirs > 1 )) ; then
    banner=${${:-"$(printf '%*s' $COLUMNS)"}// /*}
    choice="$(printf '%s\n' $banner "Create $dir" $dirs $banner | dmenu -b -l 40)"
    if [[ -z $choice ]] || [[ $choice = $banner ]] ; then
      return 0
    elif [[ $choice != Create* ]] ; then
      cd $choice
      return 0
    fi
  fi
  if [[ ! -e $dir ]] ; then
    if (( $+choice )) || read -q "choice?Create $1 [y/N]? " ; then
      if mkdir -p $dir ; then
        cd $dir
      fi
    fi
  fi
}

if (( $+commands[nvm] )) ; then
  nvm-env () {
    (( $# )) || set -- "$(<.node-version)"
    eval "$(nvm env $1)"
  }
  _nvmenv () { _arguments "1:version:(($(nvm versions)))" }
  compdef _nvmenv nvm-env

  setup_nvm_version () {
    [[ -e .node-version ]] && nvm-env
  }
  setup_nvm_version
  chpwd_functions+=( setup_nvm_version )
fi

setup_real_pwd() {
  local -a actual
  actual=( "$(pwd -r)"(+A) )
  if [[ ${actual[1]} != $PWD ]]
  then real_pwd=${actual[1]//$HOME/\~}
  else unset real_pwd
  fi
}
chpwd_functions+=( setup_real_pwd )

typeset -A cdenv_vars
cd_env() {
  local line var val
  local -A vars
  if [[ -f .env.cd ]] ; then
    for line in ${(f):-"$(<.env.cd)"} ; do
      vars+=( ${line%%=*} ${line#*=} )
    done
  fi
  for var val in ${(kv)cdenv_vars} ; do
    (( $+vars[$var] )) && continue
    unset $var
    printfc 1 '-%s=%s\n' $var $val >&2
    unset "cdenv_vars[$var]"
  done
  for var val in ${(kv)vars} ; do
    if (( $+cdenv_vars[$var] )) && [[ $cdenv_vars[$var] == $val ]]
    then printf ' %s=%s\n' $var $val >&2
    else printfc 2 '+%s=%s\n' $var $val >&2
    fi
    eval "export $var=$val"
    cdenv_vars+=( $var $val )
  done
}
chpwd_functions+=( cd_env )

if (( $+functions[_version] )) ; then
  auto_version() {
    local v
    if [[ -f .ruby-version ]] ; then
      v=$(<.ruby-version)
      unset ruby_version
      _version ruby $v
      if [[ $ruby_version = $v ]]
      then notice "ruby => $v"
      else warn "No such ruby: $v"
      fi
    fi
  }
  chpwd_functions+=( auto_version )
fi

chpwd_functions_ls() {
  [[ $zsh_eval_context[1] == toplevel ]] || return 0
  printf 'CONTEXT[%s]\n' "${zsh_eval_context[@]}"
  printf '%s[%s]\n' SHLVL "$SHLVL"
  ls -tlA --color=always |
  sed '2,11!d' |
  tac
}
chpwd_functions+=( chpwd_functions_ls )

local cmd
for cmd in $chpwd_functions ; do $cmd ; done

preso() {
  [[ -f $preso_file ]] && rm $preso_file || touch $preso_file
  setup_preso true
}
(( ! $+VIM )) && setup_preso

if (( $INLIN )) &&
  (( $+SSH_CLIENT )) &&
  (( ${#${(f):-"$(ps --ppid ${PPID:-0})"}} == 2 ))
then ssh_control_master=true
fi

# Ctrl+Ins -> send current buffer to clipboard
zle-clip-line() { printf '%s' $BUFFER | clip &> /dev/null }
zle -N zle-clip-line
bindkey '^[[2^' zle-clip-line
bindkey '^[[2;5~' zle-clip-line

# copy interrupted lines to the yank ring
zle-copy-interrupted() {
  if [[ -n $ZLE_LINE_ABORTED ]]
  then zle copy-region-as-kill -- $ZLE_LINE_ABORTED
  fi
  unset ZLE_LINE_ABORTED
}

# Set up bracketed paste mode in terminals that handle it
if [[ $TERM == (rxvt-unicode*|xterm*) ]]
then
  enable-bracketed-paste() { printf '\e[?2004h' }
  disable-bracketed-paste() { printf '\e[?2004l' }
else
  enable-bracketed-paste() {}
  disable-bracketed-paste() {}
fi

zle-line-init() {
  zle-copy-interrupted
  enable-bracketed-paste
}

zle-line-finish() {
  disable-bracketed-paste
}

# kill bracketed-paste highlighting
zle_highlight=(paste:)

load-namedirs --quiet
