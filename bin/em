if expr "$TERM" : ".*-24bit$" >/dev/null && test -d ~/.emacs.d/term
then better="$TERM-super"
elif test -d ~/.emacs.d/term
then better="$TERM"
else better=xterm-256color
fi
env TERM=$better emacs -nw "$@"
