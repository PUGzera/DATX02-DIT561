module Base (
  runGhc,
  preludeModuleName,
  daisonModuleName
) where

import qualified GHCInterface as GHC

preludeModuleName, daisonModuleName :: GHC.ModuleName
preludeModuleName = GHC.mkModuleName "Prelude"
daisonModuleName  = GHC.mkModuleName "Database.Daison"

runGhc :: GHC.Ghc a -> IO a
runGhc = GHC.runGhc (Just GHC.libdir)
