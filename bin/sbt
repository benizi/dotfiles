#!/bin/zsh
: ${sbt_version=0.13.1}
url=http://repo.typesafe.com/typesafe/ivy-releases/org.scala-sbt/sbt-launch/$sbt_version/sbt-launch.jar
dest_dir=$HOME/bin.local
dest=$dest_dir/$url:t

[[ -d $dest_dir ]] || mkdir $dest_dir
[[ -f $dest ]] || curl -o $dest $url
java -X{ms512,mx1536,ss1}M -XX:{+CMSClassUnloadingEnabled,MaxPermSize=256M} -jar $dest "$@"
