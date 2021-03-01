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

      loadModules $ map makeIIDecl [preludeModuleName, daisonModuleName, ioClassModuleName]

      res <- liftGhc $ GHC.execStmt stmt GHC.execOptions
      return $ case res of
        GHC.ExecComplete {GHC.execResult = Right _} -> (Just res)
        GHC.ExecComplete {GHC.execResult = Left e}  -> E.throw e
        _                                           -> (Nothing)
