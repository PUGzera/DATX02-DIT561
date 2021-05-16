-- | Base of the program. Open databases and settings such as GHC flags are held in 'DaisonState'.
module Frontend.Base (
  DaisonI(..),
  DaisonState(..),
  DaisonIError(..),
  baseModuleNames,
  baseExtensions,
  runGhc,
  emptyState,
  getState,
  modifyState,
  liftGhc,
  modifyFlags
) where

import qualified Frontend.GHCInterface as GHC

import Database.Daison

import qualified Control.Exception as E
import Control.Monad (liftM)
import Control.Monad.IO.Class
import Data.IORef (IORef, newIORef)
import Data.Typeable (Typeable)


-- | Represents the active state of the program.
data DaisonState = DaisonState {
    mode :: AccessMode, -- ^ The set mode to access the database. ReadWriteMode/ReadOnlyMode
    activeDB :: Maybe String, -- ^ The set active database. Queries will run to a database with this name if set.
    openDBs :: [String], -- ^ A list of all databases that are currently open
    modules :: [GHC.InteractiveImport], -- ^ List of imported modules
    flags :: Maybe GHC.DynFlags, -- ^ Extra flags which GHC commands are run with
    input :: Bool -> String -> IO (Maybe String), -- ^ Latest input from the user
    logInput :: Bool, -- ^ Log input to allow arrow key navigation
    logPath :: Maybe FilePath, -- ^ Path to log file
    currentDirectory :: String -- ^ Current working directory
}

-- | Acts as a state transformer for 'DaisonState'.
-- Replicates a mutable state.
newtype DaisonI a = DaisonI{ exec :: DaisonState -> GHC.Ghc (a, DaisonState) }

-- | Used to display errors.
data DaisonIError = DBNotOpen
                  | NoLogFile
                  | NoOpenDB
                  | NoSuchDir
                  | InvalidAccessMode String
                  | UnknownCmd String
                      deriving Typeable

-- | Represents an empty state with nothing set.
emptyState :: DaisonState
emptyState = DaisonState ReadWriteMode Nothing [] [] Nothing (\_ _ -> return Nothing) False Nothing "" -- may need to get actual current directory

-- | Returns the current state in the `DaisonI` monad.
getState :: DaisonI DaisonState
getState = DaisonI $ \st -> return (st, st)

-- | Alter the state somehow.
-- Can change active DB as an example.
modifyState :: (DaisonState -> DaisonState) -> DaisonI ()
modifyState f = DaisonI $ \st -> return ((), f st)

-- | Change the current list of flags
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
        (v, st') <- exec x st
        exec (f v) st'

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
            (reflectDaisonI st m ref)
            $ \e -> reflectDaisonI st (h e) ref

    gmask f =
        DaisonI $ \st -> do
            ref <- getSessionRef
            GHC.liftIO $ GHC.gmask $ \io_restore ->
                let
                    g_restore ds = do
                        (v,_) <- GHC.liftIO $ io_restore $ reflectDaisonI st ds ref
                        return v
                in
                    reflectDaisonI st (f g_restore) ref

instance Show DaisonIError where
    show DBNotOpen = "Database has not been opened"
    show NoLogFile = "No log file detected"
    show NoOpenDB = "No open database found. Try :open <name>"
    show NoSuchDir = "No such directory"
    show (InvalidAccessMode arg) = arg ++ " is not a valid AccessMode"
    show (UnknownCmd cmd) = "Unknown command " ++ cmd

instance E.Exception DaisonIError

instance Show AccessMode where
    show ReadWriteMode = "ReadWriteMode"
    show ReadOnlyMode = "ReadOnlyMode"

-- | Lift GHC functions to DaisonI.
liftGhc :: GHC.Ghc a -> DaisonI a
liftGhc m = DaisonI $ \st -> do a <- m; return (a, st)

-- | List of modules which are imported on startup.
baseModuleNames :: [GHC.ModuleName]
baseModuleNames = map GHC.mkModuleName [
    "Prelude",
    "Database.Daison",
    "Control.Monad.IO.Class",
    "Data.Data",
    "System.Directory"
    ]

-- | List of GHC extensions which are imported on startup.
baseExtensions :: [GHC.Extension]
baseExtensions = [
    GHC.MonadComprehensions,
    GHC.DeriveDataTypeable
    ]

-- | Run GHC functions in DaisonI.
runGhc :: DaisonState -> DaisonI a -> IO (a, DaisonState)
runGhc state ds = GHC.runGhc (Just GHC.libdir) (exec ds state)

getSessionRef :: GHC.Ghc (IORef GHC.HscEnv)
getSessionRef = do
    session <- GHC.getSession
    GHC.liftIO $ newIORef session

reflectDaisonI :: DaisonState -> DaisonI a -> IORef GHC.HscEnv -> IO (a, DaisonState)
reflectDaisonI state ds ref = GHC.reflectGhc (exec ds state) $ GHC.Session ref
