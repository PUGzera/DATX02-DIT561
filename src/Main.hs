module Main (
  main
) where

import Frontend.Base
import Frontend.Context
import Frontend.Eval
import qualified Frontend.GHCInterface as GHC

import Database.Daison
import System.Console.Haskeline

import Data.List
import Control.Monad.Catch (catch)
import Control.Concurrent (myThreadId)
import System.Posix.Signals

instance Show AccessMode where
    show ReadWriteMode = "ReadWriteMode"
    show ReadOnlyMode = "ReadOnlyMode"

-- | Ensure run is not in a half-active state after CTRL+C when run in GHCi
main :: IO ()
main = do
    this <- myThreadId
    installHandler keyboardSignal (Catch (GHC.throwTo this GHC.UserInterrupt)) Nothing
    run

run :: IO ()
run = do
    state <- return $ DaisonState ReadWriteMode "test.db" [] Nothing
    runGhc state loop 
    return ()

loop :: DaisonI (Maybe GHC.ExecResult, DaisonState)
loop = do
    state <- getState

    let settings = defaultSettings {historyFile = Just "daison_history"}
    let ts = "do {db <- openDB \"" ++ db state ++ "\"; " 
                ++ "res <- runDaison db " ++ show (mode state) 
                ++ "(do " -- ++ stmt
    let tf =  ");  closeDB db;}"

    res <- GHC.liftIO $ runInputT settings $ getInputLine $ "Daison (" ++ db state ++ ")> "
    do
        case res of
            Nothing -> return (Nothing, state)
            Just "" -> loop
            Just "q" -> return (Nothing, state)
            Just "quit" -> return (Nothing, state)
            Just input | "db " `isPrefixOf` input -> do
                modifyState $ \st -> st {db = words input !! 1}
                loop
            Just input | "import " `isPrefixOf` input -> do
                addImport' (words input !! 1) -- TODO: load modules here.
                                              -- This throws an error now. Also, it doesn't handle the case when input is ""
                loop
            Just stmt -> do
                runStmt $ ts ++ stmt ++ tf
                loop
        `catch`
        handleError state

handleError :: DaisonState -> GHC.SomeException -> DaisonI (Maybe GHC.ExecResult, DaisonState)
handleError state e = do 
        do
            GHC.liftIO $ print (e :: GHC.SomeException)
            loop
        `catch` 
        \e -> do
            GHC.liftIO $ print $ (e :: GHC.AsyncException)
            return (Nothing, state)
