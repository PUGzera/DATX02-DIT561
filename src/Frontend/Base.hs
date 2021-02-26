module Frontend.Base (
  DaisonI(..),
  DaisonState(..),
  preludeModuleName,
  daisonModuleName,
  runGhc
) where

import qualified Frontend.GHCInterface as GHC

import Database.Daison

data DaisonState = DaisonState {
    mode :: AccessMode,
    db :: String
}

data DaisonI a = DaisonI { exec :: DaisonState -> GHC.Ghc a }

instance Monad DaisonI where
    return x  = DaisonI $ \st -> return x
    (>>=) x f = DaisonI $ \st -> do
        v <- (exec x) st
        (exec (f v)) st

instance Applicative DaisonI where
    pure  = return
    (<*>) fs as = do
        f <- fs
        a <- as
        pure (f a)

instance Functor DaisonI where

preludeModuleName, daisonModuleName :: GHC.ModuleName
preludeModuleName = GHC.mkModuleName "Prelude"
daisonModuleName  = GHC.mkModuleName "Database.Daison"

runGhc :: DaisonState -> DaisonI a -> IO a
runGhc state ds = GHC.runGhc (Just GHC.libdir) ((exec ds) state)
