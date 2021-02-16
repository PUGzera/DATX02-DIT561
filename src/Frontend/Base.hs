module Base (
  preludeModuleName,
  daisonModuleName
) where

import qualified GHCInterface as GHC

import qualified Control.Exception as E

preludeModuleName, daisonModuleName :: GHC.ModuleName
preludeModuleName = GHC.mkModuleName "Prelude"
daisonModuleName  = GHC.mkModuleName "Database.Daison"

runGhcDaison :: GHC.Ghc ()
runGhcDaison = GHC.initGhcMonad (Just GHC.libdir)

runGhc :: GHC.Ghc a -> IO a
runGhc = GHC.runGhc (Just GHC.libdir)

loadModules :: [GHC.InteractiveImport] -> GHC.Ghc ()
loadModules is = do
  ctx <- GHC.getContext
  GHC.setContext (is ++ ctx)

makeIIModule :: GHC.ModuleName -> GHC.InteractiveImport
makeIIModule = GHC.IIModule

makeIIDecl :: GHC.ModuleName -> GHC.InteractiveImport
makeIIDecl = GHC.IIDecl . GHC.simpleImportDecl

runStmt :: String -> GHC.Ghc (Maybe GHC.ExecResult)
runStmt stmt = do
      dflags <- GHC.getSessionDynFlags
      GHC.setSessionDynFlags dflags
      GHC.liftIO $ print "step 1"
      loadModules $ map makeIIDecl [preludeModuleName, daisonModuleName]
      GHC.liftIO $ print "step 2"
      res <- GHC.execStmt stmt GHC.execOptions
      GHC.liftIO $ print "step 3"
      return $ case res of
        GHC.ExecComplete {GHC.execResult = Right _} -> Just res
        GHC.ExecComplete {GHC.execResult = Left e}  -> E.throw e
        _                                           -> Nothing


main :: IO ()
main = do
    runGhc (runStmt "openDB \"hej.db\"")
    return ()
