module Eval (
  display,
  runStmt
) where

import qualified GHCInterface as GHC

import qualified Control.Exception as E
import qualified System.Process as P

import Prelude

import Base
import Context

-- | Send string to the 'less' command via the 'echo' command, making it 
-- navigable with the arrow keys.
-- If this is not possible, print normally (e.g. when run from the Windows 
-- command-line).
display :: String -> IO ()
display string = do
    res <- E.catch (display' string) $
                   \e -> (return . Left . show) (e :: E.IOException)
    case res of
        Left _ -> putStrLn string
        Right () -> return ()

display' :: String -> IO (Either String ())
display' string = do
    (_, Just hout, _, _) <- 
        P.createProcess(P.proc "echo" [string]) { P.std_out = P.CreatePipe }
    (_, _, _, hcmd) <- 
        P.createProcess(P.proc "less" []) { P.std_in = P.UseHandle hout }
    P.waitForProcess hcmd
    return $ Right ()

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

