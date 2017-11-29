#!/bin/sh

cd `dirname $0`
exec ghci -isrc:lib:app:test:../engine/src:../engine/lib "$@"
