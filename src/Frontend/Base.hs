module Base (
  preludeModuleName,
  daisonModuleName
) where

import qualified GHCInterface as GHC

import Database.Daison

import qualified Control.Exception as E

instance GHC.HasDynFlags Daison where
    getDynFlags = GHC.getDynFlags

instance GHC.ExceptionMonad Daison where
    gcatch = GHC.gcatch
    gmask = GHC.gmask

instance GHC.GhcMonad Daison where
  getSession = GHC.getSession
  setSession = GHC.setSession


preludeModuleName, daisonModuleName :: GHC.ModuleName
preludeModuleName = GHC.mkModuleName "Prelude"
daisonModuleName  = GHC.mkModuleName "Database.Daison"

runGhcDaison :: Daison ()
runGhcDaison = GHC.initGhcMonad (Just GHC.libdir)


loadModules :: [GHC.InteractiveImport] -> Daison ()
loadModules is = do
  ctx <- GHC.getContext
  GHC.setContext (is ++ ctx)

makeIIModule :: GHC.ModuleName -> GHC.InteractiveImport
makeIIModule = GHC.IIModule

makeIIDecl :: GHC.ModuleName -> GHC.InteractiveImport
makeIIDecl = GHC.IIDecl . GHC.simpleImportDecl

runStmt :: String -> Daison (Maybe GHC.ExecResult)
runStmt stmt = do
      dflags <- GHC.getSessionDynFlags
      GHC.setSessionDynFlags dflags

      loadModules $ map makeIIDecl [preludeModuleName, daisonModuleName]

      res <- GHC.execStmt stmt GHC.execOptions
      return $ case res of
        GHC.ExecComplete {GHC.execResult = Right _} -> Just res
        GHC.ExecComplete {GHC.execResult = Left e}  -> E.throw e
        _                                           -> Nothing

