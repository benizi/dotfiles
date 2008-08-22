bindkey -e
bindkey "^T" expand-word
setopt nohup no_notify no_check_jobs
setopt auto_cd no_auto_menu
setopt extended_glob glob_dots hist_subst_pattern 2>/dev/null
setopt pushd_silent
export MAILCHECK=0
autoload -U compinit
uname | grep -q CYGWIN && export INCYG=true
if [ -n "$INCYG" ] ; then
	export NO_UTF8_TITLE=true
	compinit -i
	cd
else
	compinit
fi

dirs=(~/.zsh-scripts ~ ~/.zsh-scripts-)
SCRIPT=${(%)${:-%N}}
if [ -L $SCRIPT ] ; then
	SCRIPT=$(readlink $SCRIPT)
	dirs+=($SCRIPT:h)
fi
for dir in $dirs ; do
	[ ! -d $dir ] && continue
	setopt nullglob
	pushd $dir
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
