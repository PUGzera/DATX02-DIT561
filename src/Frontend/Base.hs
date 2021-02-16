module Base (
  DaisonI(..),
  DaisonState(..),
  preludeModuleName,
  daisonModuleName,
  runGhc
) where

import qualified GHCInterface as GHC

import Database.Daison

data DaisonState = DaisonState {
    mode :: AccessMode,
    db :: String
}

data DaisonI a = DaisonI { exec :: DaisonState -> GHC.Ghc (a, DaisonState) }

getState :: DaisonI DaisonState
getState = DaisonI $ \st -> return (st, st)

modifyState :: (DaisonState -> DaisonState) -> DaisonI ()
modifyState f = DaisonI $ \st -> return ((), f st)

instance Monad DaisonI where
    return x  = DaisonI $ \st -> return (x, st)
    (>>=) x f = DaisonI $ \st -> do
        (v, _) <- (exec x) st
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

runGhc :: DaisonState -> DaisonI a -> IO (a, DaisonState)
runGhc state ds = GHC.runGhc (Just GHC.libdir) ((exec ds) state)
