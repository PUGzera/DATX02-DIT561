module Eval (
  runStmt
) where

import qualified GHCInterface as GHC

import qualified Control.Exception as E

import Prelude

import Base
import Context

import Database.Daison

-- | Run statements from Prelude and Daison in the 'DaisonI' monad.
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

run :: IO ()
run = do
    print "write DB name"
    dbName <- getLine
    let state = DaisonState ReadWriteMode dbName
    runGhc state $ do
        st <- getState
        runStmt $ "db <- openDB \"" ++ db st ++ ".db\""
        query <- GHC.liftIO getLine --Todo: make sure to use runDaison db mode then every query
        --let stmt = "runDaison db " ++ show (mode st) ++ " $ do " ++ query Todo: add show function for access mode
        runStmt "2+2"
    return ()

test :: IO ()
test = do
    runGhc (DaisonState ReadWriteMode "") (runStmt "openDB \"hej.db\"")
    return ()
