module Frontend.Eval (
  display,
  runExpr
) where

import qualified Frontend.GHCInterface as GHC

import qualified System.Process as P

import Prelude

import Frontend.Base
import Frontend.Context
import Frontend.Typecheck


-- | Send string to the 'less' command via the 'echo' command, making it 
-- navigable with the arrow keys.
-- If this is not possible, print normally (e.g. when run from the Windows 
-- command-line).
display :: Show a => a -> DaisonI ()
display showable = do
    let sendToLess = do
            (_, Just hout, _, _) <- 
                P.createProcess(P.proc "echo" [show showable]) { P.std_out = P.CreatePipe }
            (_, _, _, hcmd) <- 
                P.createProcess(P.proc "less" []) { P.std_in = P.UseHandle hout }
            P.waitForProcess hcmd
            return $ Right ()

    res <- GHC.liftIO $ GHC.catch sendToLess $
            \e -> (return . Left . show) (e :: GHC.IOException)
    GHC.liftIO $ case res of
        Left _ -> putStrLn . show $ showable
        Right () -> return ()

-- | Run expressions in the DaisonI monad.
runExpr :: String -> DaisonI [GHC.Name]
runExpr expr = do
    category <- getExprCategory expr
    case category of
        Just "Statement"   -> runStmt expr
        Just "Declaration" -> runDecl expr
        _                  -> return []

-- | Run declarations in the DaisonI monad.
runDecl :: String -> DaisonI [GHC.Name]
runDecl = liftGhc . GHC.runDecls

-- | Run statements in the DaisonI monad.
runStmt :: String -> DaisonI [GHC.Name]
runStmt stmt = do
    res <- liftGhc $ GHC.execStmt stmt GHC.execOptions
    case GHC.execResult res of
        Left error  -> GHC.throw error
        Right names -> return names
