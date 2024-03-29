-- | Evaluates expressions and runs them in GHC.
module Frontend.Eval (
  display,
  display',
  getResult,
  getResults,
  runExpr,
  runExpr',
  runDaisonStmt,
  runDecl',
  runStmt'
) where

import qualified Frontend.GHCInterface as GHC
import Frontend.Base
import Frontend.Format
import Frontend.Typecheck
import Frontend.Util (wipeFile)

import qualified System.Process as P
import System.IO (hClose, hPutStrLn, openTempFile)

import Data.Char (isSymbol)

-- | Send a printable value to the 'less' command via the 'echo' command, 
--   making it navigable with the arrow keys.
--   If this is not possible, or if the string is short, print normally 
--   (e.g. when run from the Windows command-line).
display :: Show a => a -> DaisonI ()
display = display'' False

-- | Same as display, but send to 'less' even if the string is short.
display' :: Show a => a -> DaisonI ()
display' = display'' True

display'' :: Show a => Bool -> a -> DaisonI ()
display'' forceLess showable = do
    let string = show showable
    let sendToLess fp = do
            (_, _, _, hcmd) <- 
                P.createProcess $ P.shell ("less -f --chop-long-lines " ++ fp)
            P.waitForProcess hcmd
            return $ Right ()

    if isShort string && not forceLess then GHC.liftIO $ print showable
    else do
        dir <- currentDirectory <$> getState
        (fp, h) <- GHC.liftIO $ openTempFile dir "temp_daison_table.txt"
        GHC.liftIO $ hPutStrLn h string
        GHC.liftIO $ hPutStrLn h "" -- avoid use of ++ operator 
        GHC.liftIO $ hClose h

        res <- GHC.liftIO $ GHC.catch (sendToLess fp) $
                \e -> (return . Left . show) (e :: GHC.IOException)
        wipeFile True fp
        GHC.liftIO $ case res of
            Left _ -> print showable
            Right () -> return ()

-- | Determine if the string representation for a value is short, with
--   respect to character count.
isShort :: String -> Bool
isShort str = length str < charThreshold
    where charThreshold = 80

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

-- | Run an expression in the DaisonI monad and return string
--   representations of the result.
runExpr' :: String -> DaisonI [String]
runExpr' = runRetStr runExpr

-- | Run an expression in the DaisonI monad.
runExpr :: String -> DaisonI [GHC.Name]
runExpr expr = do
    category <- getExprCategory expr
    case category of
        Just "Statement"   -> runStmt expr
        Just "Declaration" -> runDecl expr
        Nothing            -> runStmt expr -- Will print an error message

-- | Perform a Daison transaction.
--   Throws an exception if no database has been opened.
--   Displays the result in a navigable format if it is not short.
runDaisonStmt :: String -> DaisonI [GHC.Name]
runDaisonStmt stmt = do
    state <- getState
    t <- exprType stmt
    let query = "it <- runDaison _activeDB "
                ++ show (mode state) ++ " "
                ++ "(" ++ stmt ++ ")"
    case activeDB state of
        Nothing -> GHC.throw NoOpenDB
        Just _  -> do
            out <- runExpr query
            res <- getResults out
            formatTable (head res) stmt >>= display
            return out

-- | Run a declaration in the DaisonI monad and return string
--   representations of the result.
runDecl' :: String -> DaisonI [String]
runDecl' = runRetStr runDecl

-- | Run a declaration in the DaisonI monad.
--   Due to changes in newer versions of GHC (>8.6.5), declarations of the
--   form "x = y" need to be converted to the statement 'let x = y' in order
--   to function as intended.
runDecl :: String -> DaisonI [GHC.Name]
runDecl expr = 
    if isVariableAssignment expr
        then runStmt $ "let " ++ expr
        else (liftGhc . GHC.runDecls) expr

-- | Check if a string assigns a value to one or more variables.
--   Returns True if this is the case.
isVariableAssignment :: String -> Bool
isVariableAssignment expr = containsAssignmentOperator expr && noDeclKeywords expr

-- | Check if a string contains the assignment operator "=".
--   Returns True when an equals sign occurs without any symbols surrounding it.
containsAssignmentOperator :: String -> Bool
containsAssignmentOperator = cAO' "aaa"
    where
        cAO' str@(a:'=':c:"") ""
            | noSurroundingSymbols str = True
            | otherwise                = False
        cAO' str@(a:'=':c:"") expr'
            | noSurroundingSymbols str = True
            | otherwise                = cAO' (newStr str expr') (newExpr expr')
        cAO' str expr' = cAO' (newStr str expr') (newExpr expr')

        noSurroundingSymbols (a:b:c:"") = (not . all isSymbol) $ a:c:""
        newStr str expr' = tail str ++ [head expr']
        newExpr = tail

-- | Returns True if the string does not start with a declaration keyword.
--   Ignores leading whitespace.
noDeclKeywords :: String -> Bool
noDeclKeywords expr = head (words expr) `notElem` declKeywords

-- | Contains the keywords that make up declarations other than the
--   `x = y` declaration, according to the GHCI user guide.
declKeywords :: [String]
declKeywords = ["data", "type", "newtype", "class", "instance",
                "deriving", "foreign"]

-- | Run a statement in the DaisonI monad and return string representations 
--   of the result.
runStmt' :: String -> DaisonI [String]
runStmt' = runRetStr runStmt

-- | Run a statement in the DaisonI monad.
runStmt :: String -> DaisonI [GHC.Name]
runStmt stmt = do
    res <- liftGhc $ GHC.execStmt stmt GHC.execOptions
    case GHC.execResult res of
        Left error  -> GHC.throw error
        Right names -> return names

-- | Return string representations of the result instead of GHC.Names.
runRetStr :: (String -> DaisonI [GHC.Name]) -> (String -> DaisonI [String])
runRetStr runF expr = do
    names <- runF expr
    getResults names
