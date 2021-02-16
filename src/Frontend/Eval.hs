module Eval (
  runStmt
) where

import qualified GHCInterface as GHC

import Control.Exception

import Prelude

import Base
import Context

-- | Run statements from Prelude and Daison in the 'Ghc' monad.
-- To run statements in the 'IO' monad, do `liftIO $ runGhc $ runStmt stmt`.
--
-- TODO: Save expressions and variables in memory between sessions.
runStmt :: String -> GHC.Ghc (Maybe GHC.ExecResult)
runStmt stmt = do
      dflags <- GHC.getSessionDynFlags
      GHC.setSessionDynFlags dflags

      loadModules $ map makeIIDecl [preludeModuleName, daisonModuleName]

      res <- GHC.execStmt stmt GHC.execOptions
      return $ case res of
        GHC.ExecComplete {GHC.execResult = Right _} -> Just res
        GHC.ExecComplete {GHC.execResult = Left e}  -> throw e
        _                                           -> Nothing
