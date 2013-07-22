#!/bin/sh

if [ ! -f .vagrant ]
then
  echo "No .vagrant file found"
  [ -f Vagrantfile ] && echo Try running vagrant up
  exit 1
fi

uuid=$(ruby -rjson -e 'puts JSON.parse(File.read(".vagrant"))["active"]["default"]')

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
