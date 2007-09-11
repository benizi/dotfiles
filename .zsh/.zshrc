bindkey -e
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
	if [ -f $dir/.ZSHFILES ]
	then for file in $(cat $dir/.ZSHFILES) ; [[ -r $dir/$file ]] && . $dir/$file
	else for file in $dir/*zsh_*~*.swp~*.zsh_history ; [[ -r $file ]] && . $file
	fi
	setopt nonullglob
done
[[ -f ~/.zshreminder ]] && cat ~/.zshreminder
