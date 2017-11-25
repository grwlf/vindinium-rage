#!/bin/sh

exts() {
   for e in FlexibleContexts ExistentialQuantification \
           ScopedTypeVariables MultiWayIf TemplateHaskell \
           RecordWildCards OverloadedStrings LambdaCase \
           Rank2Types NondecreasingIndentation DeriveGeneric \
           DeriveFunctor BangPatterns TemplateHaskell \
           FunctionalDependencies FlexibleInstances; do
    echo -X$e
   done
}

exec ghci -isrc:lib `exts` "$SB" "$@"
