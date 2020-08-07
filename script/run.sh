#/usr/bin/env bash

if [ ! -d "$(pwd)/.cabal-sandbox" ]; then
  cabal v1-sandbox init
fi

cabal v1-install --offline --only-dependencies --dry-run 2>&1 1> /dev/null
if [ $? -ne 0 ]; then
  cabal v1-update
  cabal v1-install --only-dependencies
fi

cabal v1-run exe:ReaCSS