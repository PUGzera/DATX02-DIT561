module Context (
  loadModules,
  makeIIModule,
  makeIIDecl
) where

import qualified GHC as GHC

import Exception

import Base

loadModules :: [GHC.InteractiveImport] -> DaisonI ()
loadModules is = DaisonI $ \st -> do
  ctx <- GHC.getContext
  GHC.setContext (is ++ ctx)

makeIIModule :: GHC.ModuleName -> GHC.InteractiveImport
makeIIModule = GHC.IIModule

makeIIDecl :: GHC.ModuleName -> GHC.InteractiveImport
makeIIDecl = GHC.IIDecl . GHC.simpleImportDecl

addImport :: String -> DaisonI ()
addImport mod = 
  loadModules [makeIIModule $ GHC.mkModuleName mod]