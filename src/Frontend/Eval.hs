module Eval (
  runStmt
) where

import qualified GHCInterface as GHC

import Control.Exception

import Prelude
import Base
import Context

runStmt :: String -> IO (Maybe GHC.ExecResult)
runStmt stmt =
  -- GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut $ do
    runGhc $ do
      dflags <- GHC.getSessionDynFlags
      GHC.setSessionDynFlags dflags

      loadModules $ map makeIIDecl [preludeModuleName, daisonModuleName]

      res <- GHC.execStmt stmt GHC.execOptions
      return $ case res of
        GHC.ExecComplete {GHC.execResult = Right _} -> Just res
        GHC.ExecComplete {GHC.execResult = Left e}  -> throw e
        _                                           -> Nothing
