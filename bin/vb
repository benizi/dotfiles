#!/bin/sh

file="$(find .vagrant -maxdepth 0 -type f | grep . || find .vagrant -name id -type f)"

found="$(printf '%s\n' "$file" | grep -c .)"
if test $found -eq 0
then
  echo "No .vagrant file found"
  [ -f Vagrantfile ] && echo Try running vagrant up
  exit 1
elif test $found -gt 1
then
  echo "Can't determine which vagrant box to modify"
  exit 1
fi

if test $file = '.vagrant'
then uuid=$(ruby -rjson -e 'puts JSON.parse(File.read(".vagrant"))["active"]["default"]')
else uuid=$(cat $file)
fi

case $1 in
  id) printf '%s\n' $uuid ;;
  snapshot) vbargs="snapshot $uuid take snap-$(date +%Y-%m-%d-%H%M%S)" ;;
  poweroff) vbargs="controlvm $uuid $1" ;;
  restore|revert) vbargs="snapshot $uuid restorecurrent" ;;
  *)
    echo "Usage: vb ( id | poweroff | [restore|revert] )"
    exit 1
    ;;
esac

if [ -n "$vbargs" ]
then VBoxManage $vbargs
fi
