if expr "$TERM" : ".*-24bit$" >/dev/null
then better="$TERM-super"
else better="$TERM"
fi
env TERM=$better emacs -nw "$@"
