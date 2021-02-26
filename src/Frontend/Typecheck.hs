module Typecheck (
    exprType
) where

import qualified GHCInterface as GHC
import Base
import Context

import System.IO (Handle)

import Database.Daison (AccessMode(..))

exprType :: String -> DaisonI String
exprType expr = do
  dflags <- liftGhc GHC.getSessionDynFlags
  liftGhc $ GHC.setSessionDynFlags dflags    -- must be set to find modules to be imported

  loadModules $ map makeIIDecl [preludeModuleName, daisonModuleName, ioClassModuleName]

  t <- liftGhc $ GHC.exprType GHC.TM_Inst expr
  unqual <- liftGhc GHC.getPrintUnqual
  return $ GHC.showSDocForUser dflags unqual (GHC.pprTypeForUser t)

-- Currently not used
typeToStr :: GHC.Type -> DaisonI String
typeToStr t = do
  dflags <- liftGhc GHC.getSessionDynFlags
  unqual <- liftGhc GHC.getPrintUnqual
  return $ GHC.showSDocForUser dflags unqual (GHC.pprTypeForUser t)

-- TODO: put in another module
test expr = do
  (s,_) <- GHC.liftIO $ runGhc (DaisonState ReadWriteMode "" [] Nothing) $ exprType expr
  return s
