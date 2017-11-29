#!/bin/sh

exec ghci -isrc:lib:app:../engine/src:../engine/lib "$SB" "$@"
