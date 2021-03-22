-- | Evaluates expressions and runs them in GHC.
module Frontend.Eval (
  display,
  getResult,
  getResults,
  runExpr,
  runExpr'
) where

import qualified Frontend.GHCInterface as GHC

import qualified System.Process as P

import Prelude

import Frontend.Base
import Frontend.Context
import Frontend.Typecheck

import Data.Char (isSymbol)

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

-- | Get string representations of the results from e.g. runExpr.
getResults :: [GHC.Name] -> DaisonI [String]
getResults = mapM getResult

-- | Get a string representation of the value corresponding to a Name
getResult :: GHC.Name -> DaisonI String
getResult name = do
    dflags <- liftGhc GHC.getSessionDynFlags

    mtt <- liftGhc $ GHC.lookupName name
    let ttId = case mtt of
            Just tt -> GHC.tyThingId tt
            Nothing -> error "name not found"
    term <- liftGhc $ GHC.obtainTermFromId maxBound True ttId

    return $ GHC.showSDoc dflags $ GHC.ppr term

-- | Run expressions in the DaisonI monad and return a string
--   representation of the result.
runExpr' :: String -> DaisonI [String]
runExpr' expr = do
    names <- runExpr expr
    getResults names

-- | Run expressions in the DaisonI monad.
runExpr :: String -> DaisonI [GHC.Name]
runExpr expr = do
    category <- getExprCategory expr
    case category of
        Just "Statement"   -> runStmt expr
        Just "Declaration" -> runDecl expr
        _                  -> return []

-- | Run declarations in the DaisonI monad.
--   Due to changes in newer versions of GHC (>8.6.5), declarations of the
--   form "x = y" need to be converted to the statement 'let x = y' in order
--   to function as intended.
runDecl :: String -> DaisonI [GHC.Name]
runDecl expr = case containsAssignmentOperator expr of
        True -> runStmt $ "let " ++ expr
        False -> (liftGhc . GHC.runDecls) expr

-- | Check if a string contains the assignment operator "=".
--   Returns true when an equals sign occurs without any symbols surrounding it.
containsAssignmentOperator :: String -> Bool
containsAssignmentOperator expr = cAO' "aaa" expr
    where
        cAO' str@(a:'=':c:"") ""
            | noSurroundingSymbols str = True
            | otherwise                = False
        cAO' str@(a:'=':c:"") expr'
            | noSurroundingSymbols str = True
            | otherwise                = cAO' (newStr str expr') (newExpr expr')
        cAO' str expr' = cAO' (newStr str expr') (newExpr expr')

        noSurroundingSymbols (a:b:c:"") = (not . all id . map isSymbol) $ a:c:""
        newStr str expr' = tail str ++ [head expr']
        newExpr expr' = tail expr'

-- | Run statements in the DaisonI monad.
runStmt :: String -> DaisonI [GHC.Name]
runStmt stmt = do
    res <- liftGhc $ GHC.execStmt stmt GHC.execOptions
    case GHC.execResult res of
        Left error  -> GHC.throw error
        Right names -> return names
