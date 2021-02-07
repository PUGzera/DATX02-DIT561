{-# LANGUAGE MonadComprehensions #-}
import Database.Daison
import System.Environment

import GHC
import Control.Monad.IO.Class
import DynFlags
import Outputable
import GhcMonad
import GHC.IO

import Exception


instance HasDynFlags Daison where

instance ExceptionMonad Daison where

instance GhcMonad Daison where
  getSession = GHC.getSession
  setSession = GHC.setSession

run :: Daison ()
run = do 
  stmt <- GhcMonad.liftIO getLine
  res <- compileExpr stmt
  GhcMonad.liftIO $ print "log"
  GhcMonad.liftIO $print res
  return ()

people_name :: Index (String,Int) String
people_name = index people "name" fst

people :: Table (String,Int)
people = table "people"
           `withIndex` people_name

{-main = do
  db <- openDB "test.db"
  x <- runDaison db ReadWriteMode $ do
         --dropTable people
         tryCreateTable people
         insert people (return ("Aga",15))
         insert people (return ("Aga",15))
         --update people2 (\_ (name,age,x) -> (name,age,10))
           --             (from people2)
         select [x | x <- from people everything]
  print x
  closeDB db
-}

main = do
  db <- openDB "test.db"
  runDaison db ReadWriteMode run
