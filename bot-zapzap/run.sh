#!/bin/sh

while true ; do
  if test -f _stop ; then break ; fi
  echo "Create './_stop' file to stop the bot gracefully"

  REV=`git rev-parse HEAD | cut -b '1-7'`
  NCM=`git rev-list --count HEAD | xargs printf "%04d"`
  ./result/bin/vindinium-zapzap --tag="zapzap-$NCM-$REV" "$@"

  echo "Graceful pause"
  sleep 10
done
