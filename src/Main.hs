module Main (
  main
) where

import Frontend.Base
import Frontend.Context
import Frontend.Eval

import Database.Daison

main :: IO ()
main = do
    runGhc (DaisonState ReadWriteMode "hej") (runStmt "openDB \"hej.db\"")
    return ()
