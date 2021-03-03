module Frontend.Typecheck (
    exprType,
    exprIsQuery
) where

import qualified Frontend.GHCInterface as GHC hiding (catch)
import Frontend.Base
import Frontend.Context

import System.IO (Handle)
import Control.Monad.Catch

import Database.Daison (AccessMode(..))

exprType :: String -> DaisonI String
exprType expr = do
  dflags <- liftGhc GHC.getSessionDynFlags

  t <- liftGhc $ GHC.exprType GHC.TM_Inst expr
  unqual <- liftGhc GHC.getPrintUnqual
  return $ GHC.showSDocForUser dflags unqual (GHC.pprTypeForUser t)

-- TODO: check if this is enough
exprIsQuery :: String -> DaisonI Bool
exprIsQuery expr = do
    t <- catch (exprType expr) ignoreAssignmentError

    case Just t >>= startsWithLowerM >>= isDaison of
        Nothing -> return True  -- run in Daison monad
        Just t  -> return False -- run as pure/IO expression

    where
        -- Assumes that "let a = b" is the only reasonable input that
        -- raises the "not an expression" error
        ignoreAssignmentError :: GHC.SourceError -> DaisonI String
        ignoreAssignmentError e = do
            let msg = show e
            case take 18 msg of
                "not an expression:" -> return msg
                _ -> GHC.liftIO $ GHC.throwIO e

        startsWithLowerM :: String -> Maybe String
        startsWithLowerM ('m':str) = Nothing
        startsWithLowerM str       = Just str

        isDaison :: String -> Maybe String
        isDaison str
            | take 6 str == "Daison" = Nothing
            | otherwise              = Just str

-- Currently not used
typeToStr :: GHC.Type -> DaisonI String
typeToStr t = do
  dflags <- liftGhc GHC.getSessionDynFlags
  unqual <- liftGhc GHC.getPrintUnqual
  return $ GHC.showSDocForUser dflags unqual (GHC.pprTypeForUser t)

-- TODO: put in another module
test expr = do
  (s,_) <- GHC.liftIO $ runGhc (DaisonState ReadWriteMode "" [] [] Nothing) $ exprType expr
  return s
