{-# LANGUAGE CPP #-}

-- | The loop of the program.
module Frontend.Run (
  run, initSession
) where

import Frontend.Base
import Frontend.Context
import Frontend.Eval
import Frontend.Typecheck
import Frontend.Util
import qualified Frontend.GHCInterface as GHC

import System.Environment

import System.Directory

import Database.Daison

import Data.Maybe

import Data.List
import Control.Concurrent (myThreadId)

#if !defined(mingw32_HOST_OS) && !defined(TEST)
import System.Posix.Signals
#endif

instance Show AccessMode where
    show ReadWriteMode = "ReadWriteMode"
    show ReadOnlyMode = "ReadOnlyMode"

-- | Start a session with initial values and then wait for user input.
run :: (String -> IO (Maybe String)) -> IO ()
run input = do
#if !defined(mingw32_HOST_OS) && !defined(TEST)
    -- (Non-Windows)
    -- Ensure run is not in a half-active state after CTRL+C when run in GHCi
    this <- myThreadId
    installHandler keyboardSignal (Catch (GHC.throwTo this GHC.UserInterrupt)) Nothing
#endif
    d <- getCurrentDirectory
    state <- return $ DaisonState ReadWriteMode Nothing [] [] Nothing input d
    runGhc state $ do
        initSession
        loop `GHC.gfinally` closeDBs
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
    runExpr "let _openDBs = [] :: [(String, Database)]"
    return ()

loop :: DaisonI ()
loop = do
    state <- getState

    res <- GHC.liftIO $ input state $ getPrompt state
    case res of
        Nothing      -> cmdQuit
        Just ""      -> loop
        Just ":dbs"  -> cmdListOpenDBs
        Just ":q"    -> cmdQuit
        Just ":quit" -> cmdQuit
        Just input
            | ":close "  `isPrefixOf` input -> cmdClose input
            | ":db "     `isPrefixOf` input -> cmdOpen input
            | ":import " `isPrefixOf` input -> cmdImport input
            | ":open "   `isPrefixOf` input -> cmdOpen input
            | ":t "      `isPrefixOf` input -> cmdType input
            | ":cd "     `isPrefixOf` input -> cmdCd input
            | ":set "     `isPrefixOf` input -> cmdSet input
            | otherwise                     -> cmdExpr input
        `GHC.gcatch`
            handleError state

closeDBs :: DaisonI ()
closeDBs = do
    state <- getState
    mapM_ (runExpr . sCloseDB) (openDBs state)

getPrompt :: DaisonState -> String
getPrompt state =
    case activeDB state of
        Nothing   -> "Daison> "
        Just file -> "Daison (" ++ file ++ ")> "

removeCmd :: String -> String
removeCmd = unwords . tail . words

removeDoubleQuotes :: String -> String
removeDoubleQuotes = filter (/= '"')

cmdQuit :: DaisonI ()
cmdQuit = return ()

cmdListOpenDBs :: DaisonI ()
cmdListOpenDBs = do
    state <- getState
    GHC.liftIO $ print $ openDBs state
    loop

setStartupExtensions :: DaisonI ()
setStartupExtensions = do
    args <- GHC.liftIO getArgs
    let exts = map (\e -> readExtension $ drop 2 e) (filter (\a -> "-X" `isPrefixOf` a) $ map (\i -> removeDoubleQuotes $ (words i) !! 1 )args)
    mapM_ addExtension $ catMaybes exts
    loop


cmdSet :: String -> DaisonI ()
cmdSet input = do
    let arg = removeDoubleQuotes $ (words input) !! 1
    case ("X" `isPrefixOf` arg) of
        True -> do
            case readExtension $ drop 1 arg of
                Just ext -> do
                    addExtension ext
                    loop
                Nothing -> do
                    GHC.liftIO $ print "no extension with that name exists"
                    loop
        False -> do
            GHC.liftIO $ print "not an extension name"
            loop

-- | Updates the current directory
cmdCd :: String -> DaisonI ()
cmdCd input = do
    let arg = removeDoubleQuotes $ (words input) !! 1
    cd arg
    st <- getState
    runExpr $ "setCurrentDirectory \"" ++ (currentDirectory st) ++ "\""
    loop

-- | Opens a database within the session and marks it as active,
--   while keeping track of other open databases.
--   Already opened databases will not be reopened.
cmdOpen :: String -> DaisonI ()
cmdOpen input = do
    state <- getState
    let arg = removeDoubleQuotes $ words input !! 1
    let dbs = openDBs state
    runExpr $ "_activeDB <- openDB \"" ++ arg ++ "\""
    if arg `elem` dbs
        then modifyState $ \st -> st{activeDB = Just arg}
        else do
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
    addImport $ makeIIDecl $ GHC.mkModuleName $ removeCmd input
    loop

cmdType :: String -> DaisonI ()
cmdType input = do
    let arg = removeCmd input
    t <- exprType arg
    GHC.liftIO $ putStrLn $ arg ++ " :: " ++ t
    loop

cmdExpr :: String -> DaisonI ()
cmdExpr expr = do
    isQuery <- exprIsQuery expr
    if isQuery then runDaisonStmt expr else runExpr expr -- TODO: Find a way to read the result outside of the session
    loop

-- | Perform a Daison transaction.
--   Throws an exception if no database has been opened.
runDaisonStmt :: String -> DaisonI [GHC.Name]
runDaisonStmt stmt = do
    state <- getState
    t <- exprType stmt
    daisonStmt <- mToDaison stmt
    let query = "it <- runDaison _activeDB " 
                ++ show (mode state) ++ " " 
                ++ "$ (" ++ daisonStmt ++ ")"
    case activeDB state of
        Nothing -> GHC.throw NoOpenDB
        Just _  -> do
            runExpr query
            runExpr "it"

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
    runExpr $ "let temp_" ++ var ++ " = " ++ newValue
    runExpr $ "let " ++ var ++ " = temp_" ++ var
    return () 

{- Functions to be used as part of runExpr arguments -}

-- | Within session: Database
--   Opens a database.
sOpenDB :: String -> String
sOpenDB fileName = "openDB \"" ++ fileName ++ "\""

-- | Within session: IO ()
--   Closes a database.
sCloseDB :: String -> String
sCloseDB fileName = "closeDB $ " ++ sGetDB (Just fileName)

-- | Within session: Database
--   Returns a Database from the list of opened databases.
sGetDB :: Maybe String -> String
sGetDB Nothing = "\"\""
sGetDB (Just fileName) = "snd . head $ filter (\\(x,_) -> x == \"" ++ fileName ++ "\") _openDBs"

-- | Within session: [(String, Database)]
--   Add the active database to a list of opened databases.
--   Make sure to do sOpenDB first!
sAddDB :: String -> String
sAddDB fileName = "(\"" ++ fileName ++ "\", _activeDB) : _openDBs"

-- | Within session: [(String, Database)]
--   Removes a database from the list of opened databases.
--   Make sure to do sCloseDB first!
sRemoveDB :: String -> String
sRemoveDB fileName = "filter (\\(x,_) -> x /= \"" ++ fileName ++ "\") _openDBs"
