bindkey -e
bindkey "^T" expand-word
bindkey "^R" history-incremental-pattern-search-backward
setopt nohup no_notify no_check_jobs
setopt auto_cd no_auto_menu
setopt extended_glob glob_dots hist_subst_pattern 2>/dev/null
setopt pushd_silent
setopt no_recexact no_rcquotes
export MAILCHECK=0
autoload -U compinit
export ZSH_UNAME=$(uname)
export ZSH_UNAMER=$(uname -r | perl -lpwe 's/^(\d+(?:\.\d+)?)\D.*$/$1/')
case $ZSH_UNAME in
	CYGWIN_NT-6.1) export INWIN7=true ;|
	*CYGWIN*) export INCYG=true ;;
	*FreeBSD*) export INBSD=true ;;
	*) export INLIN=true ;;
esac
if [ -n "$INCYG" ] ; then
	compinit -i
	cd
else
	compinit
fi
[ -n "$INCYG" -a -n "$INWIN7" ] && export CYGWIN=nontsec

_pre_dirs=(~/.zsh-scripts)
_post_dirs=(~/.zsh-scripts-)
zsh_dirs=(~)
typeset -U zsh_dirs
SCRIPT=${(%):-"%N"}
zsh_dirs+=( $SCRIPT:h $SCRIPT:A:h )
zsh_dirs=( $_pre_dirs ${^zsh_dirs}{,.local,-}(N/) $_post_dirs )
zsh_dirs=( ${^zsh_dirs}(N/) )
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
for l in '' 's' ; [[ -f ~/.zshreminder$l ]] && cat ~/.zshreminder$l
#screen -ls 2>/dev/null | grep -q Detached && exec screen -rr
#[ -n "$SSH_CLIENT" ] && [ "$TERM" != "screen" ] && exec screen -RR -- zsh -l
