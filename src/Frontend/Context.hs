-- | Handles the GHC Context. Imports/extensions
module Frontend.Context (
  loadModules,
  makeIIModule,
  makeIIDecl,
  addImport,
  addImport',
  addExtension
) where

import qualified Frontend.GHCInterface as GHC
import Frontend.Base

-- | Add a list of modules to the current context.
loadModules :: [GHC.InteractiveImport] -> DaisonI ()
loadModules is = do
  ctx <- liftGhc GHC.getContext
  liftGhc $ GHC.setContext (is ++ ctx)
  return ()

-- | Helper function. Create module from 'GHC.ModuleName'.
makeIIModule :: GHC.ModuleName -> GHC.InteractiveImport
makeIIModule = GHC.IIModule

-- | Helper function. Create declaration from 'GHC.ModuleName'.
makeIIDecl :: GHC.ModuleName -> GHC.InteractiveImport
makeIIDecl = GHC.IIDecl . GHC.simpleImportDecl

-- | Helper function. Converts a String to 'GHC.InteractiveImport' and adds it to the session.
addImport' :: String -> DaisonI ()
addImport' mod =
  loadModules [makeIIModule $ GHC.mkModuleName mod]

-- | Add a 'GHC.InteractiveImport' to the session.
addImport :: GHC.InteractiveImport -> DaisonI ()
addImport im = do
    modifyState $ \st -> st { modules = im : modules st }
    st <- getState
    liftGhc $ GHC.setContext (modules st)

-- | Add a 'GHC.Extension' to the session.
addExtension :: GHC.Extension -> DaisonI ()
addExtension ext = do
    st <- getState
    dflags <- liftGhc GHC.getSessionDynFlags
    modifyFlags $ GHC.xopt_set dflags ext
