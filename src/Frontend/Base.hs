module Base (
  DaisonI(..),
  DaisonState(..),
  preludeModuleName,
  daisonModuleName,
  ioClassModuleName,
  runGhc,
  getState,
  modifyState,
  liftGhc,
  modifyFlags
) where

import qualified GHCInterface as GHC

import Database.Daison

import Control.Monad.Catch
import Control.Monad.IO.Class

import qualified Control.Exception as E


data DaisonState = DaisonState {
    mode :: AccessMode,
    db :: String,
    modules :: [GHC.InteractiveImport],
    flags :: Maybe GHC.DynFlags
}

data DaisonI a = DaisonI { exec :: DaisonState -> GHC.Ghc (a, DaisonState) }

getState :: DaisonI DaisonState
getState = DaisonI $ \st -> return (st, st)

modifyState :: (DaisonState -> DaisonState) -> DaisonI ()
modifyState f = DaisonI $ \st -> return ((), f st)

modifyFlags :: GHC.DynFlags -> DaisonI ()
modifyFlags dflags = do
    modifyState $ \st -> st { flags = Just dflags }
    st <- getState
    case flags st of
        Just f -> do liftGhc $ GHC.setSessionDynFlags f; return ()
        _      -> return ()

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

instance MonadThrow DaisonI where
    throwM = GHC.liftIO . GHC.throwIO

instance MonadCatch DaisonI where
    catch m h = DaisonI $ \s -> do
        GHC.liftIO $ GHC.catch
            (do
                v <- runGhc s m 
                return v
            )
            $ \e -> do
                v <- runGhc s (h e) 
                return v

liftGhc :: GHC.Ghc a -> DaisonI a
liftGhc m = DaisonI $ \st -> do a <- m; return (a, st)

preludeModuleName, daisonModuleName :: GHC.ModuleName
preludeModuleName = GHC.mkModuleName "Prelude"
daisonModuleName  = GHC.mkModuleName "Database.Daison"
ioClassModuleName = GHC.mkModuleName "Control.Monad.IO.Class"

runGhc :: DaisonState -> DaisonI a -> IO (a, DaisonState)
runGhc state ds = GHC.runGhc (Just GHC.libdir) ((exec ds) state)
