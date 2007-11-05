bindkey -e
bindkey "^T" expand-word
setopt nohup no_notify no_check_jobs
setopt auto_cd no_auto_menu
setopt extended_glob glob_dots
setopt pushd_silent
export MAILCHECK=0
autoload -U compinit
compinit

for dir in ~/.zsh-scripts ~ ~/.zsh-scripts-
do if [ ! -d $dir ] ; then continue ; fi
	setopt nullglob
	pushd $dir
	if [ -f .ZSHFILES ]
	then for file in $(cat .ZSHFILES) ; [[ -r $file ]] && source $file
	else for file in *zsh_*~*.swp~*.zsh_history ; [[ -r $file ]] && source $file
	fi
	popd
	setopt nonullglob
done
[[ -f ~/.zshreminder ]] && cat ~/.zshreminder
