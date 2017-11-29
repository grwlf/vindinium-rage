#!/bin/sh

find engine/ bot-zipzip/ -name '*hs' | haskdogs -f -

# Lens hack
sed -i 's/_\(.*\)$/&\
\1/' tags
