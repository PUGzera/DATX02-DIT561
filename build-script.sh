#!/bin/sh

cabal sandbox init # a no-op if the sandbox exists.
cabal sandbox add-source ../daison
cabal install --dependencies-only
cabal build