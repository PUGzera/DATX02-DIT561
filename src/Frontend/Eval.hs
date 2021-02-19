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
runStmt stmt = DaisonI $ \st -> do
      dflags <- GHC.getSessionDynFlags
      GHC.setSessionDynFlags dflags

      exec (loadModules $ map makeIIDecl [preludeModuleName, daisonModuleName]) st

      res <- GHC.execStmt stmt GHC.execOptions
      return $ case res of
        GHC.ExecComplete {GHC.execResult = Right _} -> (Just res, st)
        GHC.ExecComplete {GHC.execResult = Left e}  -> E.throw e
        _                                           -> (Nothing, st)

readQuery :: DaisonI ()
readQuery = do
    st <- getState --use to get db and access mode later
    query <- GHC.liftIO getLine
    res <- runStmt query
    return ()
