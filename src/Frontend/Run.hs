{-# LANGUAGE CPP #-}
module Frontend.Run (
  run
) where

import Frontend.Base
import Frontend.Context
import Frontend.Eval
import Frontend.Typecheck
import qualified Frontend.GHCInterface as GHC

import Database.Daison

import Data.List
import Control.Monad.Catch (catch)
import Control.Concurrent (myThreadId)

#if !defined(mingw32_HOST_OS) && !defined(TEST)
import System.Posix.Signals
#endif

instance Show AccessMode where
    show ReadWriteMode = "ReadWriteMode"
    show ReadOnlyMode = "ReadOnlyMode"

run :: (String -> IO (Maybe String)) -> IO ()
run input = do
#if !defined(mingw32_HOST_OS) && !defined(TEST)
    -- (Non-Windows)
    -- Ensure run is not in a half-active state after CTRL+C when run in GHCi
    this <- myThreadId
    installHandler keyboardSignal (Catch (GHC.throwTo this GHC.UserInterrupt)) Nothing
#endif

    state <- return $ DaisonState ReadWriteMode Nothing [] [] Nothing input
    runGhc state $ do
        initSession
        loop
    return ()

{- Within the session:
   _activeDB  :: Database
   _openDBs   :: [(String, Database)]
-}
initSession :: DaisonI ()
initSession = do
    dflags <- liftGhc GHC.getSessionDynFlags
    liftGhc $ GHC.setSessionDynFlags dflags
    mapM_ addImport $ map makeIIDecl baseModuleNames
    mapM_ addExtension baseExtensions
    runExpr $ "let _openDBs = [] :: [(String, Database)]"
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
            | otherwise                     -> cmdExpr input
        `catch`
            handleError state

getPrompt :: DaisonState -> String
getPrompt state = do
    case activeDB state of
        Nothing   -> "Daison> "
        Just file -> "Daison (" ++ file ++ ")> "

removeCmd :: String -> String
removeCmd = unwords . tail . words

removeDoubleQuotes :: String -> String
removeDoubleQuotes = filter (\ch -> ch /= '"')

cmdQuit :: DaisonI ()
cmdQuit = do
    state <- getState
    sequence_ $ map (\database -> runExpr (sCloseDB database)) $ openDBs state

cmdListOpenDBs :: DaisonI ()
cmdListOpenDBs = do
    state <- getState
    GHC.liftIO $ print $ openDBs state
    loop

-- | Opens a database within the session and marks it as active,
--   while keeping track of other open databases.
--   Already opened databases will not be reopened.
cmdOpen :: String -> DaisonI ()
cmdOpen input = do
    state <- getState
    let arg = removeDoubleQuotes $ (words input) !! 1
    let dbs = openDBs state
    runExpr $ "_activeDB <- openDB \"" ++ arg ++ "\""
    case arg `elem` dbs of
        True -> do
            modifyState $ \st -> st{activeDB = Just arg}
        False -> do
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
    let arg = removeDoubleQuotes $ (words input) !! 1
    let dbs = openDBs state
    case arg `elem` dbs of
        True -> do
            let dbs' = filter (\str -> str /= arg) dbs
            let newActive = case dbs' of
                    [] -> Nothing
                    _ -> Just $ head dbs'
            runExpr $ sCloseDB arg 
            updateSessionVariable "_openDBs" $ sRemoveDB arg
            runExpr $ "let _activeDB = " ++ sGetDB newActive
            modifyState $ \st -> st{activeDB = newActive,
                                    openDBs = dbs'}
            loop
        False -> GHC.throw DBNotOpen

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
    case isQuery of
        True -> runDaisonStmt expr -- TODO: Find a way to read the result outside of the session
        False -> runExpr expr
    loop

-- | Perform a Daison transaction.
--   Throws an exception if no database has been opened.
runDaisonStmt :: String -> DaisonI [GHC.Name]
runDaisonStmt stmt = do
    state <- getState
    t <- exprType stmt
    daisonStmt <- mToDaison stmt
    let query = "runDaison _activeDB " 
                ++ show (mode state) ++ " " 
                ++ "$ (" ++ daisonStmt ++ ")"
    case activeDB state of
        Nothing -> GHC.throw NoOpenDB
        Just _  -> runExpr query

handleError :: DaisonState -> GHC.SomeException -> DaisonI ()
handleError state e = do 
        do
            GHC.liftIO $ print (e :: GHC.SomeException)
            loop
        `catch` 
        \e -> do
            GHC.liftIO $ print $ (e :: GHC.AsyncException)
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
