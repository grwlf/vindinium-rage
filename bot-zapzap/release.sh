#!/bin/sh

cry() {
  echo "$@" >&2
  sendmail "ierton@gmail.com" <<EOF
Vindinium update script is exiting with:
$@
`cat nix-build.log`
EOF
}

set -x
while true ; do

  git fetch

  if git branch -v | grep behind ; then
    git reset --hard origin/master || oops "Reset failed"
    {
      nix-build
    } >nix-build.log 2>&1 ||
      cry "Falied to build the bot"
  fi

  sleep 5

done
