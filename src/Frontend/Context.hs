module Context (
  loadModules,
  makeIIModule,
  makeIIDecl
) where

import qualified GHC as GHC

import Base

loadModules :: [GHC.InteractiveImport] -> DaisonI ()
loadModules is = do
  ctx <- liftGhc GHC.getContext
  liftGhc $ GHC.setContext (is ++ ctx)
  return ()

makeIIModule :: GHC.ModuleName -> GHC.InteractiveImport
makeIIModule = GHC.IIModule

makeIIDecl :: GHC.ModuleName -> GHC.InteractiveImport
makeIIDecl = GHC.IIDecl . GHC.simpleImportDecl
