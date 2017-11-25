#!/bin/sh

while true ; do
  if test -f _stop ; then break ; fi
  echo "Create './_stop' file to stop the bot gracefully"

  ./dist/build/vindinium-zapzap/vindinium-zapzap "$@"

  echo "Graceful pause"
  sleep 10
done
