{-# LANGUAGE CPP, BangPatterns #-}

-- | The loop of the program.
module Frontend.Run (
  run, initSession
) where

import qualified Frontend.GHCInterface as GHC
import Frontend.Base
import Frontend.Context
import Frontend.Eval
import Frontend.Typecheck
import Frontend.Util

import qualified System.IO as SIO
import System.Environment (getArgs)
import System.Directory (getCurrentDirectory)

import Control.Concurrent (myThreadId)
import Control.Monad (replicateM_)
import Data.Maybe (fromMaybe)
import Data.List (isPrefixOf)

#if !defined(mingw32_HOST_OS) && !defined(TEST)
import System.Posix.Signals
#endif

-- | Start a session with initial values and then wait for user input.
run :: Maybe FilePath -> (Bool -> String -> IO (Maybe String)) -> IO ()
run logFilePath input = do
#if !defined(mingw32_HOST_OS) && !defined(TEST)
    -- (Non-Windows)
    -- Ensure run is not in a half-active state after CTRL+C when run in GHCi
    this <- myThreadId
    installHandler keyboardSignal (Catch (GHC.throwTo this GHC.UserInterrupt)) Nothing
#endif
    d <- getCurrentDirectory
    let state = emptyState{input=input, logInput=True, logPath=logFilePath,
                           currentDirectory=d}
    runGhc state $ do
        initSession
        printText welcomeMsg
        setStartupArgs `GHC.gfinally` exit
    return ()

{- Within the session:
   _activeDB  :: Database
   _openDBs   :: [(String, Database)]
-}
-- | Set default values for the session.
initSession :: DaisonI ()
initSession = do
    dflags <- liftGhc GHC.getSessionDynFlags
    liftGhc $ GHC.setSessionDynFlags dflags
    mapM_ (addImport . makeIIDecl) baseModuleNames
    mapM_ addExtension baseExtensions
    runExpr sDefineOpenDBs
    return ()

loop :: DaisonI ()
loop = do
    state <- getState

    res <- GHC.liftIO $ input state (logInput state) $ getPrompt state
    case res of
        Nothing      -> cmdQuit
        Just ""      -> loop
        Just ":?"    -> cmdPrintHelp
        Just ":help" -> cmdPrintHelp
        Just ":dbs"  -> cmdListOpenDBs
        Just ":q"    -> cmdQuit
        Just ":quit" -> cmdQuit
        Just input
            | ":close "  `isPrefixOf` input -> cmdClose input
            | ":db "     `isPrefixOf` input -> cmdOpen input
            | ":l "      `isPrefixOf` input -> cmdImport input
            | ":open "   `isPrefixOf` input -> cmdOpen input
            | ":t "      `isPrefixOf` input -> cmdType input
            | ":cd "     `isPrefixOf` input -> cmdCd input
            | ":set "    `isPrefixOf` input -> cmdSet input
            | ":m"       `isPrefixOf` input -> cmdModule input
            | ":log"     `isPrefixOf` input -> cmdLog input 
            | ":"        `isPrefixOf` input -> cmdError input
            | otherwise                     -> cmdExpr input
        `GHC.gcatch`
            handleError state

-- | Print exit message and close databases
exit :: DaisonI ()
exit = do
    printText exitMsg
    closeDBs

-- | Close the databases which are open, without modifying the state.
closeDBs :: DaisonI ()
closeDBs = do
    state <- getState
    mapM_ (runExpr . sCloseDB) (openDBs state)

-- | Reopen databases closed by closeDBs, keeping the same active database.
--   Assumes sDefineOpenDBs has been called prior (but after the most
--   recent call to GHC.load).
reopenDBs :: DaisonI ()
reopenDBs = do
    state <- getState
    let openAndAddDB db = do
            runExpr $ sOpenDB db
            updateSessionVariable "_openDBs" $ sAddDB db
    mapM_ openAndAddDB (openDBs state)
    runExpr . sOpenDB . fromMaybe "" $ activeDB state
    return ()

getPrompt :: DaisonState -> String
getPrompt state =
    case activeDB state of
        Nothing   -> "Daison> "
        Just file -> "Daison (" ++ file ++ ")> "

removeCmd :: String -> String
removeCmd = unwords . tail . words

removeDoubleQuotes :: String -> String
removeDoubleQuotes = filter (/= '"')

cmdError :: String -> DaisonI()
cmdError input = GHC.throw $ UnknownCmd (takeWhile (' ' /=) input)

cmdQuit :: DaisonI ()
cmdQuit = return ()

cmdListOpenDBs :: DaisonI ()
cmdListOpenDBs = do
    state <- getState
    GHC.liftIO $ print $ openDBs state
    loop

-- | Prints a help text to guide the user.
cmdPrintHelp :: DaisonI ()
cmdPrintHelp = do
    printText helpText
    loop

-- | Set extensions on startup
setStartupArgs :: DaisonI ()
setStartupArgs = do
    args <- GHC.liftIO getArgs
    flags <- liftGhc $ GHC.getSessionDynFlags
    (flags', lo, ws) <- GHC.liftIO $ GHC.parseDynamicFlagsCmdLine flags (map (\i -> GHC.L GHC.noSrcSpan i) args)
    mapM_ (\(GHC.L _ s) -> GHC.liftIO $ print $ "Unknown Argument: " ++ s) lo
    case ws of
        [] -> do
            liftGhc $ GHC.setSessionDynFlags flags'
            loop
        ws -> do
            mapM_ (\(GHC.Warn _ (GHC.L _ s)) -> GHC.liftIO $ print s) ws
            loop

-- | Set extensions
cmdSet :: String -> DaisonI ()
cmdSet input = do
    let arg = removeCmd input
    flags <- liftGhc $ GHC.getSessionDynFlags
    (flags', lo, ws) <- GHC.liftIO $ GHC.parseDynamicFlags flags [GHC.L GHC.noSrcSpan arg]
    mapM_ (\(GHC.L _ s) -> GHC.liftIO $ print $ "Unknown Flag: " ++ s) lo
    case ws of
        [] -> do
            liftGhc $ GHC.setSessionDynFlags flags'
            loop
        ws -> do
            mapM_ (\(GHC.Warn _ (GHC.L _ s)) -> GHC.liftIO $ print s) ws
            loop

-- | Updates the current directory
cmdCd :: String -> DaisonI ()
cmdCd input = do
    let arg = removeDoubleQuotes $ words input !! 1
    cd arg
    st <- getState
    runExpr $ "setCurrentDirectory \"" ++ currentDirectory st ++ "\""
    loop

-- | Opens a database within the session and marks it as active,
--   while keeping track of other open databases.
--   Already opened databases will not be reopened.
cmdOpen :: String -> DaisonI ()
cmdOpen input = do
    state <- getState
    let arg = removeDoubleQuotes $ words input !! 1
    let dbs = openDBs state
    if arg `elem` dbs
        then modifyState $ \st -> st{activeDB = Just arg}
        else do
            runExpr $ sOpenDB arg
            updateSessionVariable "_openDBs" $ sAddDB arg
            modifyState $ \st -> st{activeDB = Just arg,
                                    openDBs = arg : dbs}
    loop

-- | Closes a database within the session.
--   If this database was the active one, also marks the earliest opened
--   database as active.
--   Throws an exception if the database has not been opened.
cmdClose :: String -> DaisonI ()
cmdClose input = do
    state <- getState
    let arg = removeDoubleQuotes $ words input !! 1
    let dbs = openDBs state
    if arg `elem` dbs
        then do
            let dbs' = filter (/= arg) dbs
            let newActive = case dbs' of
                    [] -> Nothing
                    _ -> Just $ head dbs'
            runExpr $ sCloseDB arg
            updateSessionVariable "_openDBs" $ sRemoveDB arg
            runExpr $ "let _activeDB = " ++ sGetDB newActive
            modifyState $ \st -> st{activeDB = newActive,
                                    openDBs = dbs'}
            loop
        else GHC.throw DBNotOpen

cmdImport :: String -> DaisonI ()
cmdImport input = do
    target <- liftGhc $ GHC.guessTarget (removeCmd input) Nothing
    case GHC.targetId target of
        (GHC.TargetFile fp _) -> do
            closeDBs
            cm <- liftGhc $ GHC.compileToCoreModule fp
            let mName = GHC.moduleNameString $ GHC.moduleName $ GHC.cm_module cm
            liftGhc $ GHC.setTargets [target]
            res <- liftGhc $ GHC.load GHC.LoadAllTargets
            m <- liftGhc $ GHC.findModule (GHC.mkModuleName mName) Nothing
            addImport (makeIIDecl $ GHC.moduleName  m)
            runExpr sDefineOpenDBs
            reopenDBs
            loop

cmdModule :: String -> DaisonI ()
cmdModule input = do
    addImport $ makeIIDecl $ GHC.mkModuleName $ removeCmd input
    loop

cmdType :: String -> DaisonI ()
cmdType input = do
    let arg = removeCmd input
    t <- exprType arg
    GHC.liftIO $ putStrLn $ arg ++ " :: " ++ t
    loop

-- | Interact with the log file:
--   :log path   => display the log file's path
--   :log show   => display the contents of the log file
--   :log toggle => enable/disable logging (default: enabled)
--   :log wipe   => attempts to wipe the log file
--   :log _      => display a help message.
cmdLog :: String -> DaisonI ()
cmdLog input = do
    let arg = removeCmd input
    state <- getState
    cmdLog' arg state
    loop

cmdLog' "path" state = ifLogExists $ \path -> printText path

cmdLog' "show" state = ifLogExists $ \path -> do
    contents <- GHC.liftIO $ readFile path
    display' $ GHC.text contents
    
cmdLog' "toggle" state = do
    let logInput' = not $ logInput state
    modifyState $ \st -> st{logInput=logInput'}
    printText $ "Logging has been " ++ 
                if logInput' then "ENABLED." else "DISABLED." 

cmdLog' "wipe" state = ifLogExists $ \path -> do
    handle <- GHC.liftIO $ SIO.openFile path SIO.ReadWriteMode
    contents <- GHC.liftIO $ SIO.hGetContents handle
    let ![!o1,!o2] = map (replicate (length contents)) ['\xaa','\x55']
    GHC.liftIO $ SIO.hClose handle
    replicateM_ 3 $ mapM_ (GHC.liftIO . writeFile path) [o1,o2]
    GHC.liftIO $ writeFile path "" -- empty file
    printText "Log file wiped."
  
cmdLog' _ state = printText $
    ":log {path|show|toggle|wipe}\n" ++
    ":? for help"

ifLogExists :: (FilePath -> DaisonI ()) -> DaisonI () 
ifLogExists action = do
    path <- fromMaybe "" . logPath <$> getState 
    if (not . null) path
        then action path
        else GHC.throw NoLogFile

cmdExpr :: String -> DaisonI ()
cmdExpr expr = do
    isQuery <- exprIsQuery expr
    if isQuery then runDaisonStmt expr else runExpr expr
    loop

handleError :: DaisonState -> GHC.SomeException -> DaisonI ()
handleError state e =
        do
            GHC.liftIO $ print (e :: GHC.SomeException)
            loop
        `GHC.gcatch`
        \e -> do
            GHC.liftIO $ print (e :: GHC.AsyncException)
            return ()

-- | Used as a workaround to redefine a variable
--   in terms of itself
updateSessionVariable :: String -> String -> DaisonI ()
updateSessionVariable var newValue = do
    runExpr $ "let _temp" ++ var ++ " = " ++ newValue
    runExpr $ "let " ++ var ++ " = _temp" ++ var
    return ()

{- Functions to be used as part of runExpr arguments -}

-- | Within session: String
--   Surround a string with double quotes (if it has not already), 
--   so that it can be used as a runExpr argument.
sString :: String -> String
sString str@('"':_) = str
sString str         = "\"" ++ str ++ "\""

-- | Within session: N/A (_activeDB :: Database)
--   Opens a database, and marks it as active within the GHC session.
sOpenDB :: String -> String
sOpenDB fileName = "_activeDB <- openDB " ++ sString fileName

-- | Within session: IO ()
--   Closes a database.
sCloseDB :: String -> String
sCloseDB fileName = "closeDB $ " ++ sGetDB (Just fileName)

-- | Within session: Database
--   Returns a Database from the list of opened databases.
sGetDB :: Maybe String -> String
sGetDB Nothing = "\"\""
sGetDB (Just fileName) = "snd . head $ filter (\\(x,_) -> x == " ++ sString fileName ++ ") _openDBs"

-- | Within session: [(String, Database)]
--   Add the active database to a list of opened databases.
--   Make sure to do sOpenDB first!
sAddDB :: String -> String
sAddDB fileName = "(" ++ sString fileName ++ ", _activeDB) : _openDBs"

-- | Within session: [(String, Database)]
--   Removes a database from the list of opened databases.
--   Make sure to do sCloseDB first!
sRemoveDB :: String -> String
sRemoveDB fileName = "filter (\\(x,_) -> x /= " ++ sString fileName ++ ") _openDBs"

-- | Within session: N/A (_openDBs :: [(String, Database)])
--   Defines the variables needed in order to keep track of multiple databases.
sDefineOpenDBs :: String
sDefineOpenDBs = "let _openDBs = [] :: [(String, Database)]"

