{-# LANGUAGE MonadComprehensions #-}
import Database.Daison
import System.Environment

people_name :: Index (String,Int) String
people_name = index people "name" fst

people :: Table (String,Int)
people = table "people"
           `withIndex` people_name

main = do
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
