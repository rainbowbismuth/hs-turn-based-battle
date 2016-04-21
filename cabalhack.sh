#!/bin/bash

# This is for running ghc-mod correctly, so that ide-haskell works for me.
# But it is not working at the moment :(

cabal configure --package-db=clear \
                --package-db=global \
                --package-db="$(stack --no-system-ghc path --snapshot-pkg-db)" \
                --package-db="$(stack --no-system-ghc path --local-pkg-db)" \
                --with-compiler="$HOME/.stack/programs/x86_64-osx/ghc-7.10.3/bin/ghc" \
                $@
