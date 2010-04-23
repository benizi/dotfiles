bindkey -e
bindkey "^T" expand-word
(( $+widgets[history-incremental-pattern-search-backward] )) \
&& bindkey "^R" history-incremental-pattern-search-backward
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
	*SunOS*) export INSOL=true ;;
	*) export INLIN=true ;;
esac
if [ -f ~/.zcompdump.debugging ] ; then
	rm -f ~/.zcompdump
	fpath=( ~/git/zsh/Completion/**/*(N/) $fpath )
fi
compinitargs=( -d ~/.zcompdump )
[ -n "$INCYG" ] && compinitargs+=( -i )
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
