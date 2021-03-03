module Main (
  main
) where

import Frontend.Base
import Frontend.Context
import Frontend.Eval
import qualified Frontend.GHCInterface as GHC

import Database.Daison
import System.Console.Haskeline hiding (display)

import Data.List
import Control.Monad.Catch (catch)
import Control.Concurrent (myThreadId)
import System.Posix.Signals

instance Show AccessMode where
    show ReadWriteMode = "ReadWriteMode"
    show ReadOnlyMode = "ReadOnlyMode"

main :: IO ()
main = run

run :: IO ()
run = do
    -- Ensure run is not in a half-active state after CTRL+C when run in GHCi
    this <- myThreadId
    installHandler keyboardSignal (Catch (GHC.throwTo this GHC.UserInterrupt)) Nothing

    state <- return $ DaisonState ReadWriteMode "test.db" [] Nothing
    runGhc state $ do
        initSession
        loop 
    return ()

initSession :: DaisonI ()
initSession = do
    dflags <- liftGhc GHC.getSessionDynFlags
    liftGhc $ GHC.setSessionDynFlags dflags
    loadModules $ map makeIIDecl [preludeModuleName, daisonModuleName, ioClassModuleName]
    return ()

loop :: DaisonI ()
loop = do
    state <- getState
    let settings = defaultSettings {historyFile = Just "daison_history"}

    res <- GHC.liftIO $ runInputT settings $ getInputLine $ "Daison (" ++ db state ++ ")> "
    do
    case res of
        Nothing      -> return ()
        Just ""      -> loop
        Just ":q"    -> return ()
        Just ":quit" -> return ()
        Just input 
            | ":db "     `isPrefixOf` input -> cmdDB input
            | ":import " `isPrefixOf` input -> cmdImport input
            | otherwise                     -> cmdStmt input            
    `catch`
        handleError state

removeCmd :: String -> String
removeCmd = unwords . tail . words

cmdDB :: String -> DaisonI ()
cmdDB input = do
    modifyState $ \st -> st {db = removeCmd input}
    loop
    
cmdImport :: String -> DaisonI ()
cmdImport input = do
    addImport' $ removeCmd input -- TODO: load modules here.
                                 -- This throws an error now. Also, it doesn't handle the case when input is ""
    loop

cmdStmt :: String -> DaisonI ()
cmdStmt stmt = do
    state <- getState
    let ts = "do {db <- openDB \"" ++ db state ++ "\"; " 
                ++ "res <- runDaison db " ++ show (mode state) 
                ++ "(do " -- ++ stmt
    let tf =  "); closeDB db}"
    runStmt $ ts ++ stmt ++ tf
    loop

handleError :: DaisonState -> GHC.SomeException -> DaisonI ()
handleError state e = do 
        do
            GHC.liftIO $ print (e :: GHC.SomeException)
            loop
        `catch` 
        \e -> do
            GHC.liftIO $ print $ (e :: GHC.AsyncException)
            return ()