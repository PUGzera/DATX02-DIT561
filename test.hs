import GHC
import Control.Monad.IO.Class
import DynFlags
import Outputable
import GhcMonad
import GHC.IO

run :: Ghc ()
run = do
    stmt <- GhcMonad.liftIO getLine
    execStmt stmt execOptions
    return ()

main :: IO ()
main = do
    runGhc Nothing run
    return ()