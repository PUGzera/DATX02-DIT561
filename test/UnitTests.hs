{-# LANGUAGE MonadComprehensions #-}
module Main (main) where

import Test.QuickCheck
import Test.HUnit ((@?=), (@?), assertFailure, assertEqual, assertBool)
import qualified Test.HUnit as HUnit

import System.Directory (removeFile)
import System.Exit
import System.IO.Unsafe (unsafePerformIO)
import System.IO (stderr, openTempFile)
import GHC.IO.Handle

import Frontend.Base
import Frontend.Eval
import Frontend.Run (initSession)
import qualified Frontend.GHCInterface as GHC
import Frontend.Typecheck

-- Test labels
labelEvalRunDecl = "'runDecl \"t = 3\"'"
labelEvalRunExpr = "'runExpr \"1+2\"'"
labelEvalRunStmt = "'runStmt \"1+2\"'"
labelTCId     = ":t id"
labelTCIdSpec = ":t id :: Int -> Int"
labelTCIdNum  = ":t id 0"
labelTCStr1   = ":t \"id\""
labelTCStr2   = ":t \"id\" :: String"
labelQCD      = "'createTable (table [])' is a Daison query"
labelQCND1    = "'createTable' is not a Daison query"
labelQCND2    = "'print' is not a Daison query"
labelQCND3    = "'1+2' is not a Daison query"

-- Expected test values for tests that use assertEqual
expEvalRunDecl     = ["3"]
expEvalRunExpr     = ["3"]
expEvalRunStmt     = ["3"]
expTCId     = "a -> a"
expTCIdSpec = "Int -> Int"
expTCIdNum  = "Num a => a"
expTCStr1   = "[Char]"
expTCStr2   = "String"

-- Expressions to test
argEvalRunDecl  = unDaisonI $ runExpr' "test = 3"
argEvalRunExpr  = unDaisonI $ runExpr' "1+2"
argEvalRunStmt  = unDaisonI $ runExpr' "1+2"
argTCId     = unDaisonI $ exprType "id"
argTCIdSpec = unDaisonI $ exprType "id :: Int -> Int"
argTCIdNum  = unDaisonI $ exprType "id 0"
argTCStr1   = unDaisonI $ exprType "\"id\""
argTCStr2   = unDaisonI $ exprType "\"id\" :: String"
argQCD      = unDaisonI $ exprIsQuery "createTable (table [])"
argQCND1    = not $ unDaisonI $ exprIsQuery "createTable"
argQCND2    = not $ unDaisonI $ exprIsQuery "print"
argQCND3    = not $ unDaisonI $ exprIsQuery "1+2"

-- Example tests
foo x = (1,x-1)
test1 = HUnit.TestCase (assertEqual "for (foo 3)," (1,2) (foo 3))
test2 = HUnit.TestCase (assertBool "true test," True)

-- Eval Tests
-- | Check if the front-end can create a declaration.
testEvalRunDecl :: HUnit.Test
testEvalRunDecl = HUnit.TestCase $ assertEqual labelEvalRunDecl expEvalRunDecl argEvalRunDecl

-- | Check if the front-end can run an expression without specifying whether it is a declarationn or a statement.
testEvalRunExpr :: HUnit.Test
testEvalRunExpr = HUnit.TestCase $ assertEqual labelEvalRunExpr expEvalRunExpr argEvalRunExpr

-- | Check if the front-end can run a statement.
testEvalRunStmt :: HUnit.Test
testEvalRunStmt = HUnit.TestCase $ assertEqual labelEvalRunStmt expEvalRunStmt argEvalRunStmt

-- Typecheck Tests
testTCId = HUnit.TestCase $ assertEqual labelTCId expTCId argTCId 
testTCIdSpec = HUnit.TestCase $ assertEqual labelTCIdSpec expTCIdSpec argTCIdSpec
testTCIdNum = HUnit.TestCase $ assertEqual labelTCIdNum expTCIdNum argTCIdNum
testTCStr1 = HUnit.TestCase $ assertEqual labelTCStr1 expTCStr1 argTCStr1
testTCStr2 = HUnit.TestCase $ assertEqual labelTCStr2 expTCStr2 argTCStr2
testQCD = HUnit.TestCase $ assertBool labelQCD argQCD
testQCND1 = HUnit.TestCase $ assertBool labelQCND1 argQCND1
testQCND2 = HUnit.TestCase $ assertBool labelQCND2 argQCND2
testQCND3 = HUnit.TestCase $ assertBool labelQCND3 argQCND3

main :: IO ()
main = do
    c1 <- runUnitTests unitTestList
    let failures = HUnit.errors c1 + HUnit.failures c1
        exit_code
            | failures > 0 = ExitFailure failures
            | otherwise    = ExitSuccess
    exitWith exit_code

unitTestList :: HUnit.Test
unitTestList = HUnit.TestList [
    HUnit.TestLabel labelEvalRunDecl testEvalRunDecl,
    HUnit.TestLabel labelEvalRunExpr testEvalRunExpr,
    HUnit.TestLabel labelEvalRunStmt testEvalRunStmt,
    HUnit.TestLabel labelTCId testTCId,
    HUnit.TestLabel labelTCIdSpec testTCIdSpec,
    HUnit.TestLabel labelTCIdNum testTCIdNum,
    HUnit.TestLabel labelTCStr1 testTCStr1,
    HUnit.TestLabel labelTCStr2 testTCStr2,
    HUnit.TestLabel labelQCD testQCD,
    HUnit.TestLabel labelQCND1 testQCND1,
    HUnit.TestLabel labelQCND2 testQCND2,
    HUnit.TestLabel labelQCND3 testQCND3
    ]

runUnitTests :: HUnit.Test -> IO HUnit.Counts
runUnitTests = HUnit.runTestTT 

-- | Run a DaisonI action and extract its value.
{-# NOINLINE unDaisonI #-}
unDaisonI :: DaisonI a -> a
unDaisonI ds = fst $ unsafePerformIO $ runGhc emptyState (do initSession'; ds)

-- | Initializes a session, while hiding the 
--   "Loaded package environment from ..." message.
initSession' :: DaisonI ()
initSession' = do
    (fp, fh) <- GHC.liftIO $ openTempFile "test" "stderr.txt"
    stderrCopy <- GHC.liftIO $ hDuplicate stderr
    GHC.liftIO $ hDuplicateTo fh stderr
    initSession
    GHC.liftIO $ hDuplicateTo stderrCopy stderr
    GHC.liftIO $ hClose fh
    GHC.liftIO $ removeFile fp