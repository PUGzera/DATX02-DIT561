module Context (
  loadModules,
  makeIIModule,
  makeIIDecl
) where

import qualified GHC as GHC

loadModules :: GHC.GhcMonad m => [GHC.InteractiveImport] -> m ()
loadModules is = do
  ctx <- GHC.getContext
  GHC.setContext (is ++ ctx)

makeIIModule :: GHC.ModuleName -> GHC.InteractiveImport
makeIIModule = GHC.IIModule

makeIIDecl :: GHC.ModuleName -> GHC.InteractiveImport
makeIIDecl = GHC.IIDecl . GHC.simpleImportDecl
