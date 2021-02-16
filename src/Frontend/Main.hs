module Main (
  main
) where

import Base
import Context
import Eval

import Database.Daison

main :: IO ()
main = do
    runGhc (DaisonState ReadWriteMode "hej") (runStmt "openDB \"hej.db\"")
    return ()
