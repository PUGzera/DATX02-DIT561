module Context (
  loadModules,
  makeIIModule,
  makeIIDecl
) where

import qualified GHCInterface as GHC

import Exception

import Base

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

addImport :: String -> DaisonI ()
addImport mod =
  loadModules [makeIIModule $ GHC.mkModuleName mod]
