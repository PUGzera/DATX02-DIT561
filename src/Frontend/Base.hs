module Base (
  runGhc,
  preludeModuleName,
  daisonModuleName
) where

import qualified GHCInterface as GHC

import Database.Daison

instance GHC.HasDynFlags Daison where

instance GHC.ExceptionMonad Daison where

instance GHC.GhcMonad Daison where
  getSession = GHC.getSession
  setSession = GHC.setSession

preludeModuleName, daisonModuleName :: GHC.ModuleName
preludeModuleName = GHC.mkModuleName "Prelude"
daisonModuleName  = GHC.mkModuleName "Database.Daison"

runGhcDaison :: Daison a -> IO a
runGhcDaison = GHC.runGhc (Just GHC.libdir)
