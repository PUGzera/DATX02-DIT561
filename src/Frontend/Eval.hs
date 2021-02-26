module Frontend.Eval (
  runStmt
) where

import qualified Frontend.GHCInterface as GHC

import qualified Control.Exception as E

import Prelude

import Frontend.Base
import Frontend.Context

-- | Run statements from Prelude and Daison in the 'DaisonI' monad.
runStmt :: String -> DaisonI (Maybe GHC.ExecResult)
runStmt stmt = DaisonI $ \st -> do
      dflags <- GHC.getSessionDynFlags
      GHC.setSessionDynFlags dflags

      exec (loadModules $ map makeIIDecl [preludeModuleName, daisonModuleName]) st

      res <- GHC.execStmt stmt GHC.execOptions
      return $ case res of
        GHC.ExecComplete {GHC.execResult = Right _} -> Just res
        GHC.ExecComplete {GHC.execResult = Left e}  -> E.throw e
        _                                           -> Nothing
