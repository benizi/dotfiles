#!/bin/zsh
CLOJURE_DIR=~/git/clojure
CLOJURE_JAR=$CLOJURE_DIR/clojure.jar
completions_file=$CLOJURE_DIR/etc/clj_completions
rc=$HOME/.cljrc.clj
typeset -T CP cp
typeset -U cp
CP=$CLASSPATH
if [ -f project.clj ] ; then
	for dir in ${(s,:,):-"$(lein classpath)"} ; do
		cp+=( $dir )
	done
fi
cp+=( $CLOJURE_JAR $PWD )

(( $# )) && exec java -cp $CP clojure.main $1 -- $@

cmd=()
if (( ! $# )) ; then
	cmd+=( rlwrap --remember -c )
	[[ -f $completions_file ]] && cmd+=( -f $completions_file )
fi
cmd+=( java -cp $CP clojure.main )
if (( ! $# )) ; then
	[[ -f $rc ]] && cmd+=( -i $rc )
	cmd+=( --repl )
fi
# echo $cmd
exec $cmd
