#!/bin/sh

cry() {
  echo "$@" >&2
  sendmail "ierton@gmail.com" <<EOF
Vindinium update script is exiting with:
$@
`cat .cabal.log`
EOF
}

set -x
while true ; do

  git fetch

  if git branch -v | grep behind ; then
    git reset --hard origin/master || oops "Reset failed"
    {
      cabal install --only-dep
      cabal configure
      cabal build
    } >.cabal.log 2>&1 ||
      cry "Falied to cabal install the bot"
  fi

  sleep 5

done
