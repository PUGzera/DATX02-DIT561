module Main (
  main
) where

import Base
import Context
import Eval
import qualified GHCInterface as GHC

import Database.Daison
import System.Console.Haskeline

import Data.List

instance Show AccessMode where
    show ReadWriteMode = "ReadWriteMode"
    show ReadOnlyMode = "ReadOnlyMode"

main :: IO ()
main = do
    --runGhc (DaisonState ReadWriteMode "hej") (runStmt "openDB \"hej.db\"")
    return ()

run :: IO ()
run = do
    state <- return $ DaisonState ReadWriteMode "test.db" [] Nothing
    runInputT defaultSettings (loop state)
    return ()
    where
        loop :: DaisonState -> InputT IO (Maybe GHC.ExecResult, DaisonState)
        loop state = do
            let ts = "do {db <- openDB \"" ++ db state ++ "\"; " 
                      ++ "res <- runDaison db " ++ show (mode state) 
                      ++ "(do " -- ++ stmt
            let tf =  ");  closeDB db;}"

            minput <- getInputLine "Daison> "
            case minput of
                Nothing -> return (Nothing, state)
                Just "" -> loop state
                Just "q" -> return (Nothing, state)
                Just "quit" -> return (Nothing, state)
                Just input | "import" `isPrefixOf` input -> do
                    GHC.liftIO $ runGhc state $ do
                        (addImport'  input) -- TODO: load modules here.
                                            -- This throws an error now. Also, it doesn't handle the case when input is ""
                    return (Nothing, state)
                Just stmt -> do
                    GHC.liftIO $ runGhc state $ do
                        runStmt $ ts ++ stmt ++ tf
                    loop state

{-run :: IO ()
run = do
    print "write DB name"
    dbName <- getLine
    let state = DaisonState ReadWriteMode "" [] Nothing
    runGhc state $ do
        st <- getState
        runStmt $ "db <- openDB \"" ++ db st ++ ".db\""
        GHC.liftIO $ I.hSetBuffering I.stdin I.LineBuffering
        query <- GHC.liftIO getLine --Todo: make sure to use runDaison db mode then every query
        GHC.liftIO $ I.hSetBuffering I.stdin I.NoBuffering
        --let stmt = "runDaison db " ++ show (mode st) ++ " $ do " ++ query Todo: add show function for access mode
        runStmt "2+2"
    return ()
-}
