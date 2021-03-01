module Frontend.Context (
  loadModules,
  makeIIModule,
  makeIIDecl,
  addImport,
  addImport'
) where

import qualified Frontend.GHCInterface as GHC

import Exception

import Frontend.Base

import qualified Control.Exception as E

loadModules :: [GHC.InteractiveImport] -> DaisonI ()
loadModules is = do
  ctx <- liftGhc GHC.getContext
  liftGhc $ GHC.setContext (is ++ ctx)
  return ()

makeIIModule :: GHC.ModuleName -> GHC.InteractiveImport
makeIIModule = GHC.IIModule

makeIIDecl :: GHC.ModuleName -> GHC.InteractiveImport
makeIIDecl = GHC.IIDecl . GHC.simpleImportDecl

addImport' :: String -> DaisonI ()
addImport' mod =
  loadModules [makeIIModule $ GHC.mkModuleName mod]

addImport :: GHC.InteractiveImport -> DaisonI ()
addImport im = do
    modifyState $ \st -> st { modules = im:(modules st) }
    st <- getState
    liftGhc $ GHC.setContext (modules st)

-- addExtension MonadComprehension to add monad comprehension later
addExtension :: GHC.Extension -> DaisonI ()
addExtension ext = do
    st <- getState
    dflags <- liftGhc GHC.getSessionDynFlags
    modifyFlags $ GHC.xopt_set dflags ext
