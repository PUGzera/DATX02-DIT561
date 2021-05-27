-- | Typechecking of expressions. Used to determine how an expression should be executed.
module Frontend.Typecheck (
    exprType,
    exprIsQuery,
    getExprCategory,
    mToDaison
) where

import qualified Frontend.GHCInterface as GHC hiding (catch)
import Frontend.Base
import Frontend.Context

import System.IO (Handle)

import Data.List (isPrefixOf)

-- | Find the GHC evaluated expression type.
exprType :: String -> DaisonI String
exprType expr = do
  dflags <- liftGhc GHC.getSessionDynFlags
  t <- liftGhc $ GHC.exprType GHC.TM_Inst expr
  unqual <- liftGhc GHC.getPrintUnqual
  return $ GHC.showSDocForUser dflags unqual (GHC.pprTypeForUser t)

-- | Check if an expression is a Daison query
exprIsQuery :: String -> DaisonI Bool
exprIsQuery expr = do
    t <- GHC.gcatch (exprType expr) errorHandler
    let t' = removeTypeConstraint t

    if t' == ignoredError
    then return False
    else return $ startsWithLowerM t' || isDaison t'

    where
        ignoredError = "_ IgnoredError"
        errorHandler :: GHC.SourceError -> DaisonI String
        errorHandler e =
            case Just e >>= isAssignmentError >>= possibleDeclaration of
                Nothing -> return ignoredError
                Just e  -> GHC.liftIO $ GHC.throwIO e

        -- | Assumes that "let a = b" is the only reasonable input that
        --   raises the "not an expression" err
        isAssignmentError :: GHC.SourceError -> Maybe GHC.SourceError
        isAssignmentError e
            | "not an expression:" `isPrefixOf` msg = Nothing
            | otherwise                             = Just e
            where msg = show e

        startsWithLowerM :: String -> Bool
        startsWithLowerM = ("m" `isPrefixOf`)

        isDaison :: String -> Bool
        isDaison = ("Daison" `isPrefixOf`)

-- | Check if an expression treated as a statement might be a declaration,
--   and return Nothing if so.
possibleDeclaration :: GHC.SourceError -> Maybe GHC.SourceError
possibleDeclaration e
    | "parse error on input" `isPrefixOf` msg = Nothing
    | otherwise                               = Just e
    where msg = show e

-- | Categorize expression types to either declarations or statements.
getExprCategory :: String -> DaisonI (Maybe String)
getExprCategory expr = do
    dflags <- liftGhc GHC.getSessionDynFlags
    let category
            | GHC.isStmt dflags expr = Just "Statement"
            | GHC.isDecl dflags expr = Just "Declaration"
            | otherwise              = Nothing
    return category

-- | Tell the interpreter to parse 'm a' as 'Daison a'.
mToDaison :: String -> DaisonI String
mToDaison stmt = do
        t <- exprType stmt
        return $ "(" ++ stmt ++ ")"
        where
            asDaison ('m':t) = "Daison" ++ t
            asDaison t = t

-- | e.g. "QueryMonad m => m [(Key (String, Int), (String, Int))]"
--   =>   "m [(Key (String, Int), (String, Int))]"
removeTypeConstraint :: String -> String
removeTypeConstraint str
    | "=>" `elem` words str = drop 2 $ dropWhile (/= '>') str
    | otherwise               = str

-- Currently not used
typeToStr :: GHC.Type -> DaisonI String
typeToStr t = do
  dflags <- liftGhc GHC.getSessionDynFlags
  unqual <- liftGhc GHC.getPrintUnqual
  return $ GHC.showSDocForUser dflags unqual (GHC.pprTypeForUser t)
