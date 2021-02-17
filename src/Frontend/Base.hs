module Base (
  DaisonI(..),
  DaisonState(..),
  preludeModuleName,
  daisonModuleName,
  runGhc,
  getState,
  modifyState,
  liftGhc
) where

import qualified GHCInterface as GHC

import Database.Daison

import Control.Monad.IO.Class

import qualified Control.Exception as E


data DaisonState = DaisonState {
    mode :: AccessMode,
    db :: String
}

data DaisonI a = DaisonI { exec :: DaisonState -> GHC.Ghc (a, DaisonState) }

getState :: DaisonI DaisonState
getState = DaisonI $ \st -> return (st, st)

modifyState :: (DaisonState -> DaisonState) -> DaisonI ()
modifyState f = DaisonI $ \st -> return ((), f st)

instance Monad DaisonI where
    return x  = DaisonI $ \st -> return (x, st)
    (>>=) x f = DaisonI $ \st -> do
        (v, st') <- (exec x) st
        (exec (f v)) st'

instance Applicative DaisonI where
    pure  = return
    (<*>) fs as = do
        f <- fs
        a <- as
        pure (f a)

instance Functor DaisonI where

instance MonadIO DaisonI where
    liftIO m = DaisonI $ \st -> do
        v <- GHC.liftIO m
        return (v, st)

liftGhc :: GHC.Ghc a -> DaisonI a
liftGhc m = DaisonI $ \st -> do a <- m; return (a, st)

preludeModuleName, daisonModuleName :: GHC.ModuleName
preludeModuleName = GHC.mkModuleName "Prelude"
daisonModuleName  = GHC.mkModuleName "Database.Daison"

runGhc :: DaisonState -> DaisonI a -> IO (a, DaisonState)
runGhc state ds = GHC.runGhc (Just GHC.libdir) ((exec ds) state)

test :: IO ()
test = do
    runGhc (DaisonState ReadWriteMode "") (runStmt "openDB \"hej.db\"")
    return ()

runStmt :: String -> DaisonI (Maybe GHC.ExecResult)
runStmt stmt = do
      dflags <- liftGhc GHC.getSessionDynFlags
      liftGhc $ GHC.setSessionDynFlags dflags

      loadModules $ map makeIIDecl [preludeModuleName, daisonModuleName]

      res <- liftGhc $ GHC.execStmt stmt GHC.execOptions
      return $ case res of
        GHC.ExecComplete {GHC.execResult = Right _} -> (Just res)
        GHC.ExecComplete {GHC.execResult = Left e}  -> E.throw e
        _                                           -> (Nothing)

readQuery :: DaisonI ()
readQuery = do
    st <- getState --use to get db and access mode later
    query <- GHC.liftIO getLine
    res <- runStmt query
    return ()

loadModules :: [GHC.InteractiveImport] -> DaisonI ()
loadModules is = do
  ctx <- liftGhc GHC.getContext
  liftGhc $ GHC.setContext (is ++ ctx)
  return ()

makeIIModule :: GHC.ModuleName -> GHC.InteractiveImport
makeIIModule = GHC.IIModule

makeIIDecl :: GHC.ModuleName -> GHC.InteractiveImport
makeIIDecl = GHC.IIDecl . GHC.simpleImportDecl

run :: IO ()
run = do
    print "write DB name"
    dbName <- getLine
    let state = DaisonState ReadWriteMode dbName
    runGhc state $ do
        st <- getState
        runStmt $ "db <- openDB \"" ++ db st ++ ".db\""
        readQuery --Todo: make sure to use runDaison db mode then every query
    return ()