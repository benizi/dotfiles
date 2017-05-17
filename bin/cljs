#!/bin/zsh

# Allow manipulating the classpath via a tied array
typeset -T CP cp
typeset -U cp
[[ -z $CLASSPATH ]] || CP=$CLASSPATH

# Construct a path to a locally-cached Maven artifact
maven_location() {
  local -a parts
  local group=${1//./\/} artifact=$2 version=$3
  [[ -n $group ]] && [[ -n $artifact ]] || return 1
  parts+=( ~/.m2/repository $group $artifact )
  [[ -z $version ]] || parts+=( $version $artifact-$version.jar )
  printf '%s' ${(j:/:)parts}
}

# Find the newest version of a given group+artifact
maven_latest() {
  maven_location "$@" |
  xargs -0 -iDIR find DIR -mindepth 1 -maxdepth 1 -type d -print0 |
  sort -zrV |
  awk -v RS='\000' -v ORS= -F/ 'NR==1 { print $NF }'
}

# Add Maven libraries to classpath via groupId, artifactId, and version
# addcp group [artifact | artifact:version]+
addcp() {
  local item artifact version group=$1
  local -a parts
  shift
  for item
  do
    parts=( ${(s.:.)item} )
    artifact=${parts[1]}
    version=${parts[2]}
    [[ -n $version ]] || version=$(maven_latest $group $artifact)
    cp+=( "$(maven_location $group $artifact $version)" )
  done
}

# Remove HEREDOC indent (based on indentation of the first line)
strip_indent() {
  awk '!i { i = index($0,$1) } { print substr($0, i) }'
}

# Create the script to compile a single ClojureScript file
generate_buildscript() {
  local cljsversion=$1
  cat <<BUILDER | strip_indent
  ;; Only actual dependency is ClojureScript
  (def deps '[[org.clojure/clojurescript "${cljsversion}"]])

  ;; Set up the classpath according to the deps
  (require '[cemerick.pomegranate :as pom])
  (pom/add-dependencies :coordinates deps)

  ;; not sure why this version would be better/worse than the pom/ version:
  (comment
    (require '[cemerick.pomegranate.aether :as aether])
    (aether/resolve-dependencies :coordinates deps))

  ;; Compile the file from [src out] (will be passed in to Java invocation)
  (require '[cljs.build.api :as b])
  (let [[src out] *command-line-args*]
    ;(prn {:src src :out out :rest rest})
    (b/build src {:output-to out
                  :optimizations :simple ; ensures a single file
                  :target :nodejs}))
BUILDER
}

# Given a source file, compile it to a (single) destination file using Node.JS
compile_cljs() {
  local src=$1 out=$2

  # Find latest version of ClojureScript downloaded locally (changes often)
  local cljsversion=$(maven_latest org.clojure clojurescript)

  # Builds using Clojure, but needs the ClojureScript compiler
  addcp org.clojure clojure:1.8.0 clojurescript:$cljsversion

  # Pomegranate handles `pom` dependencies
  addcp com.cemerick pomegranate

  # Install the stupidly-numerous dependencies for the above
  addcp commons-logging commons-logging
  addcp org.apache.httpcomponents http{client,core}:4.1.2
  addcp org.apache.maven maven-{aether-provider,model{,-builder}}:3.0.4
  addcp org.apache.maven.wagon wagon-{http{,-shared4},provider-api}:2.2
  addcp org.codehaus.plexus plexus-utils:2.0.6 plexus-interpolation:1.14
  addcp org.sonatype.aether aether-{api,impl,spi,util,connector-{file,wagon}}:1.13.1
  addcp org.tcrawley dynapath:0.2.3

  # Warn if there are invalid items in the classpath
  for l in $cp
  do test -e $l || echo bad cp entry $l >&2
  done

  # Compile the file
  () {
    local buildscript=$1
    generate_buildscript $cljsversion > $buildscript
    java -cp $CP clojure.main $buildscript $src $out
  } =(:)
}

# Digest the contents of the ClojureScript file for caching
digest() {
  openssl dgst -sha256 -binary |
  openssl base64 -A -e |
  tr '/=' '--'
}

# Determine a content-indexed cache path for the given ClojureScript source
cache_path() {
  local src=$1
  local sha=$(digest < $src)
  local cachedir=$HOME/.cache/cljs
  printf '%s' $cachedir/$sha.js
}

# Compile the ClojureScript to JS and run it with Node
run_cljs() {
  local src=$1 out
  shift
  () {
    local tmpfile=$1
    out=$cache
    if (( $+nocljscache ))
    then out=$tmpfile
    else out=$(cache_path $src)
    fi
    mkdir -p ${out:h}
    [[ -s $out ]] || compile_cljs $src $out || exit $?
    node $out "$@"
  } =(:)
}

# Run (and cache compilation of) the ClojureScript with Lumo
run_lumo() {
  lumo "$@"
}

if (( ! $+cljsrunner ))
then
  if (( $+commands[lumo] ))
  then cljsrunner=lumo
  else cljsrunner=cljs
  fi
fi
run_$cljsrunner "$@"
