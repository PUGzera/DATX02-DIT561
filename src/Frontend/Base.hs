module Base (
  preludeModuleName,
  daisonModuleName
) where

import qualified GHCInterface as GHC

import Database.Daison

import qualified Control.Exception as E

import qualified Control.Monad.Trans.State.Lazy as St


data DaisonState = DaisonState {
    mode :: AccessMode,
    db :: String
}

data DaisonI a = DaisonI { exec :: DaisonState -> GHC.Ghc a }

instance Monad DaisonI where
    return x  = DaisonI $ \st -> return x
    (>>=) x f = DaisonI $ \st -> do
        v <- (exec x) st
        (exec (f v)) st

instance Applicative DaisonI where
    pure  = return
    (<*>) fs as = do
        f <- fs
        a <- as
        pure (f a)

instance Functor DaisonI where

preludeModuleName, daisonModuleName :: GHC.ModuleName
preludeModuleName = GHC.mkModuleName "Prelude"
daisonModuleName  = GHC.mkModuleName "Database.Daison"


runGhc :: DaisonState -> DaisonI a -> IO a
runGhc state ds = GHC.runGhc (Just GHC.libdir) ((exec ds) state)

loadModules :: [GHC.InteractiveImport] -> DaisonI ()
loadModules is = DaisonI $ \st -> do
  ctx <- GHC.getContext
  GHC.setContext (is ++ ctx)

makeIIModule :: GHC.ModuleName -> GHC.InteractiveImport
makeIIModule = GHC.IIModule

makeIIDecl :: GHC.ModuleName -> GHC.InteractiveImport
makeIIDecl = GHC.IIDecl . GHC.simpleImportDecl

runStmt :: String -> DaisonI (Maybe GHC.ExecResult)
runStmt stmt = DaisonI $ \st -> do
      dflags <- GHC.getSessionDynFlags
      GHC.setSessionDynFlags dflags
      GHC.liftIO $ print "step 1"
      exec (loadModules $ map makeIIDecl [preludeModuleName, daisonModuleName]) st
      GHC.liftIO $ print "step 2"
      res <- GHC.execStmt stmt GHC.execOptions
      GHC.liftIO $ print "step 3"
      return $ case res of
        GHC.ExecComplete {GHC.execResult = Right _} -> Just res
        GHC.ExecComplete {GHC.execResult = Left e}  -> E.throw e
        _                                           -> Nothing

main :: IO ()
main = do
    runGhc (DaisonState ReadWriteMode "hej") (runStmt "openDB \"hej.db\"")
    return ()