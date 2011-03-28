#bindkey -e

# emacs mode bindings that I still want...
bindkey "^P" up-line-or-history
bindkey "^N" down-line-or-history

bindkey "^A" beginning-of-line
bindkey "^E" end-of-line
bindkey "^F" forward-char
bindkey "^[f" forward-word
bindkey "^B" backward-char
bindkey "^[b" backward-word
bindkey "^[^?" backward-kill-word
bindkey "^K" kill-line
bindkey "^_" undo

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
autoload -Uz warn
zmodload zsh/mathfunc 2>/dev/null
if [ -f ~/.zcompdump.debugging ] ; then
	rm -f ~/.zcompdump
	fpath=( ${^zsh_dirs}/Completion(N/) ~/git/zsh/Completion/**/*(N/) $fpath )
fi
fpath+=( $^zsh_dirs/autoload(N/) )
compinitargs=( -d ~/.zcompdump )
() {
	local -a notmine
	notmine=( $fpath(N^U) )
	(( $+INCYG )) || (( $#notmine )) && compinitargs+=( -i )
}
compinit $compinitargs
[ -n "$INCYG" -a -n "$INWIN7" ] && export CYGWIN=nontsec

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
cat ${^zsh_dirs}/{.zsh,}reminder{,s} 2>/dev/null
run_local_versions ${(%):-"%x"}
#screen -ls 2>/dev/null | grep -q Detached && exec screen -rr
#[ -n "$SSH_CLIENT" ] && [ "$TERM" != "screen" ] && exec screen -RR -- zsh -l
