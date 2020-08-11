#/usr/bin/env bash

is_ide_run () {
  [ "$(basename "$(pwd)")" = "ReaCSS" ]
}

on_ide_run () {
  if [ ! -d "$(pwd)/.cabal-sandbox" ]; then
    cabal v1-sandbox init
  fi

  cabal v1-install --offline --only-dependencies --enable-tests --dry-run 2>&1 1> /dev/null
  if [ $? -ne 0 ]; then
    cabal v1-update
    cabal v1-install --only-dependencies --enable-tests -j1
  fi

  cabal v1-run exe:ReaCSS
}

on_run_only_tab () {
  rm -rf .cabal-sandbox
  cabal v1-sandbox init
  cabal v1-update
  cabal v1-install --only-dependencies --enable-tests -j1
  cabal v1-run exe:ReaCSS
}

if is_ide_run; then
  time on_ide_run
else
  time on_run_only_tab
fi