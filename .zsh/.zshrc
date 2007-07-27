bindkey -e
setopt nohup no_notify no_check_jobs
setopt auto_cd no_auto_menu
setopt extended_glob glob_dots
setopt pushd_silent
export MAILCHECK=0

for dir in ~/.zsh-scripts ~ ~/.zsh-scripts-
do if [ ! -d $dir ] ; then continue ; fi
	setopt nullglob
	for file in $dir/*zsh_*~*.swp~*.zsh_history ; [[ -r $file ]] && . $file
	setopt nonullglob
done
[[ -f ~/.zshreminder ]] && cat ~/.zshreminder
