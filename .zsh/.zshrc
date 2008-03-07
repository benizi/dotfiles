bindkey -e
bindkey "^T" expand-word
setopt nohup no_notify no_check_jobs
setopt auto_cd no_auto_menu
setopt extended_glob glob_dots
setopt pushd_silent
export MAILCHECK=0
autoload -U compinit
uname | grep -q CYGWIN && export INCYG=true
if [ -n "$INCYG" ] ; then
	compinit -i
	cd
else
	compinit
fi

for dir in ~/.zsh-scripts ~ ~/.zsh-scripts-
do if [ ! -d $dir ] ; then continue ; fi
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
