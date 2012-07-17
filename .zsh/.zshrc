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
autoload -U compinit
autoload -Uz age
() {
	local fn
	for fn in $^zsh_dirs/autoload/*(N:t) ; do
		autoload -Uz $fn
	done
}
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
[ -n "$INCYG" -a -n "$INWIN7" ] && export CYGWIN=nontsec

export LESS="-R -i -M --shift 5 -F -X -j4"
(( $+commands[lesspipe.sh] )) && export LESSOPEN="|lesspipe.sh %s"
export PAGER=less
export EDITOR=/usr/bin/vim
export READNULLCMD=$PAGER

for dir in $zsh_dirs ; do
	setopt nullglob
	pushd $dir 2> /dev/null || continue
	files=(.zshrc-)
	if [ -f .ZSHFILES ] ; then
		files=($files `cat .ZSHFILES`)
	else
		files=($files *zsh_*~*.swp~*.zsh_history)
	fi
	for file in $files ; [ -r $file ] && source $file
	popd
	setopt nonullglob
done

(( $+warn_rubyopt )) && warn "RUBYOPT was set ($warn_rubyopt)... shouldn't be"
unset warn_rubyopt
(( $+warn_rvm_root )) && warn "RVM does not play well with root - using $ruby_manager"
unset warn_rvm_root

cat ${^zsh_dirs}/{.zsh,}reminder{,s} 2>/dev/null
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

auto_git_alias () {
	[[ $1 = g* ]] || return 1
	local al=${1#g}
	shift
	git config -l | grep -qF "alias.$al=" \
		|| git help --all | awk '/---/ { ok=1 ; OFS="\n" ; ORS="" } /^ / { NF=NF+1 ; if (ok) print $0 }' | grep -qF $al \
		|| return 1
	git $al "$@"
	return 0
}
command_not_found_handlers+=( auto_git_alias )

trap '
	local dir= choose=
	set -- ${=__last_command}
	if (( $# == 1 )) && [[ $1 == */* ]] && [[ $1 != "<"* ]] ; then
		dir=${~1}
		if [[ ! -e $dir ]] ; then
			if read -q "choose?Create $1 [y/N]? " ; then
				if mkdir -p $dir ; then
					cd $dir
				fi
			fi
		fi
	fi
' ZERR
