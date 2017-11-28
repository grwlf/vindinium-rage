#!/bin/sh

exec ghci -iapp:src:lib:../engine/src:../engine/lib `exts` "$SB" "$@"
