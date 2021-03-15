module Frontend.Base (
  DaisonI(..),
  DaisonState(..),
  DaisonIError(..),
  baseModuleNames,
  baseExtensions,
  runGhc,
  getState,
  modifyState,
  liftGhc,
  modifyFlags
) where

import qualified Frontend.GHCInterface as GHC

import Database.Daison

import Control.Monad (liftM)
import Control.Monad.IO.Class
import Data.IORef
import Data.Typeable

import qualified Control.Exception as E


data DaisonState = DaisonState {
    mode :: AccessMode,
    activeDB :: Maybe String,
    openDBs :: [String],
    modules :: [GHC.InteractiveImport],
    flags :: Maybe GHC.DynFlags,
    input :: String -> IO (Maybe String),
    currentDirectory :: String
}

data DaisonI a = DaisonI { exec :: DaisonState -> GHC.Ghc (a, DaisonState) }

data DaisonIError = DBNotOpen | NoOpenDB
    deriving Typeable

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
    fmap = liftM

instance MonadIO DaisonI where
    liftIO m = DaisonI $ \st -> do
        v <- GHC.liftIO m
        return (v, st)

instance GHC.ExceptionMonad DaisonI where
    gcatch m h = DaisonI $ \st -> do
        ref <- getSessionRef
        GHC.liftIO $ GHC.catch 
            (unDaisonI st m ref)
            $ \e -> unDaisonI st (h e) ref
    
    gmask f = do
        DaisonI $ \st -> do
            ref <- getSessionRef
            GHC.liftIO $ GHC.gmask $ \io_restore ->
                let
                    g_restore ds = do
                        (v,_) <- GHC.liftIO $ io_restore $ unDaisonI st ds ref
                        return v
                in
                    unDaisonI st (f g_restore) ref

instance Show DaisonIError where
    show DBNotOpen = "database has not been opened"
    show NoOpenDB = "no open database found"
    
instance E.Exception DaisonIError

liftGhc :: GHC.Ghc a -> DaisonI a
liftGhc m = DaisonI $ \st -> do a <- m; return (a, st)

baseModuleNames :: [GHC.ModuleName]
baseModuleNames = map GHC.mkModuleName [
    "Prelude",
    "Database.Daison", 
    "Control.Monad.IO.Class",
    "Data.Data"
    ]

baseExtensions :: [GHC.Extension]
baseExtensions = [
    GHC.MonadComprehensions,
    GHC.DeriveDataTypeable
    ]

runGhc :: DaisonState -> DaisonI a -> IO (a, DaisonState)
runGhc state ds = GHC.runGhc (Just GHC.libdir) ((exec ds) state)

getSessionRef :: GHC.Ghc (IORef GHC.HscEnv)
getSessionRef = do
    session <- GHC.getSession
    GHC.liftIO $ newIORef session

unDaisonI :: DaisonState -> DaisonI a -> IORef GHC.HscEnv -> IO (a, DaisonState)
unDaisonI state ds ref = do
    GHC.reflectGhc ((exec ds) state) $ GHC.Session ref