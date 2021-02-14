module GHCInterface (module X) where

-- This is an interface towards the GHC API. Imports from GHC should only be
-- made in this module. It makes the code clean and all functions from the GHC
-- library will have the infix `GHC.`. This idea is taken from the library
-- `hint`.

import GHC as X
import GHC.Paths as X (libdir) -- maybe put this in another module since ghc-paths is a package
import DynFlags as X
