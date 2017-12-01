#!/bin/sh

cd `dirname $0`
exec ghci -isrc:gen-hs:lib:app:test:../engine/src:../engine/lib "$@"
