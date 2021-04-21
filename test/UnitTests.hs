{-# LANGUAGE MonadComprehensions #-}
-- | Unit testing for specific functions.
module Main (main) where

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
labelEvalRunDecl1 = "'runDecl \"t = 3\"'"
labelEvalRunDecl2 = "'runDecl \"[a,b,c] = [1,2,3]\"'"
labelEvalRunDecl3 = "'runDecl \"[ ((a)) , (_,b) , _ ] = [(1,2),(3,4),(5,6)]\"'"
labelEvalRunDecl4 = "'runDecl \"_ = 1+2\"'"
labelEvalRunExpr  = "'runExpr \"1+2\"'"
labelEvalRunStmt  = "'runStmt \"1+2\"'"
labelEvalCatD1 = "'data T = Test' is a declaration"
labelEvalCatD2 = "'type T = []' is a declaration"
labelEvalCatD3 = "'newtype T = Test Int' is a declaration"
labelEvalCatD4 = "'instance T' is a declaration"
labelEvalCatD5 = "'t = 3' is a declaration"
labelEvalCatD6 = "'f x = x' is a declaration"
labelEvalCatS1 = "'let t = 3' is a statement"
labelEvalCatS2 = "'let f x = x' is a statement"
labelEvalCatS3 = "'do {let f x = x in f 3}' is a statement"
labelEvalCatS4 = "'1+2' is a statement"
labelEvalCatS5 = "'createTable' is a statement"
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
expEvalRunDecl1    = ["3"]
expEvalRunDecl2    = ["1","2","3"]
expEvalRunDecl3    = ["(1,2)","4"]
expEvalRunDecl4    = []
expEvalRunExpr     = ["3"]
expEvalRunStmt     = ["3"]
expEvalCatDn = Just "Declaration"
expEvalCatSn = Just "Statement"
expTCId     = "a -> a"
expTCIdSpec = "Int -> Int"
expTCIdNum  = "Num a => a"
expTCStr1   = "[Char]"
expTCStr2   = "String"

-- Expressions to test
argEvalRunDecl1 = unDaisonI $ runDecl' "test = 3"
argEvalRunDecl2 = unDaisonI $ runDecl' "[a,b,c] = [1,2,3]"
argEvalRunDecl3 = unDaisonI $ runDecl' "[ ((a)) , (_,b) , _ ] = [(1,2),(3,4),(5,6)]"
argEvalRunDecl4 = unDaisonI $ runDecl' "_ = 1+2"
argEvalRunExpr  = unDaisonI $ runExpr' "1+2"
argEvalRunStmt  = unDaisonI $ runStmt' "1+2"
argEvalCatD1 = unDaisonI $ getExprCategory "data T = Test"
argEvalCatD2 = unDaisonI $ getExprCategory "type T = []"
argEvalCatD3 = unDaisonI $ getExprCategory "newtype T = Test Int"
argEvalCatD4 = unDaisonI $ getExprCategory "instance T"
argEvalCatD5 = unDaisonI $ getExprCategory "t = 3"
argEvalCatD6 = unDaisonI $ getExprCategory "f x = x"
argEvalCatS1 = unDaisonI $ getExprCategory "let t = 3"
argEvalCatS2 = unDaisonI $ getExprCategory "let f x = x"
argEvalCatS3 = unDaisonI $ getExprCategory "do {let f x = x in f 3}"
argEvalCatS4 = unDaisonI $ getExprCategory "1+2"
argEvalCatS5 = unDaisonI $ getExprCategory "createTable"
argTCId     = unDaisonI $ exprType "id"
argTCIdSpec = unDaisonI $ exprType "id :: Int -> Int"
argTCIdNum  = unDaisonI $ exprType "id 0"
argTCStr1   = unDaisonI $ exprType "\"id\""
argTCStr2   = unDaisonI $ exprType "\"id\" :: String"
argQCD      = unDaisonI $ exprIsQuery "createTable (table [])"
argQCND1    = not $ unDaisonI $ exprIsQuery "createTable"
argQCND2    = not $ unDaisonI $ exprIsQuery "print"
argQCND3    = not $ unDaisonI $ exprIsQuery "1+2"

-- Eval Tests
-- | Check if the front-end can create a declaration.
testEvalRunDecl1 :: HUnit.Test
testEvalRunDecl1 = HUnit.TestCase $ assertEqual labelEvalRunDecl1 expEvalRunDecl1 argEvalRunDecl1

-- | Check if the front-end can use pattern checking in assignment declarations.
testEvalRunDecl2, testEvalRunDecl3, testEvalRunDecl4 :: HUnit.Test
testEvalRunDecl2 = HUnit.TestCase $ assertEqual labelEvalRunDecl2 expEvalRunDecl2 argEvalRunDecl2
testEvalRunDecl3 = HUnit.TestCase $ assertEqual labelEvalRunDecl3 expEvalRunDecl3 argEvalRunDecl3
testEvalRunDecl4 = HUnit.TestCase $ assertEqual labelEvalRunDecl4 expEvalRunDecl4 argEvalRunDecl4

-- | Check if the front-end can run an expression without specifying whether it is a declarationn or a statement.
testEvalRunExpr :: HUnit.Test
testEvalRunExpr = HUnit.TestCase $ assertEqual labelEvalRunExpr expEvalRunExpr argEvalRunExpr

-- | Check if the front-end can run a statement.
testEvalRunStmt :: HUnit.Test
testEvalRunStmt = HUnit.TestCase $ assertEqual labelEvalRunStmt expEvalRunStmt argEvalRunStmt

-- | Check if the type checker works as intended.
testTCId = HUnit.TestCase $ assertEqual labelTCId expTCId argTCId 
testTCIdSpec = HUnit.TestCase $ assertEqual labelTCIdSpec expTCIdSpec argTCIdSpec
testTCIdNum = HUnit.TestCase $ assertEqual labelTCIdNum expTCIdNum argTCIdNum
testTCStr1 = HUnit.TestCase $ assertEqual labelTCStr1 expTCStr1 argTCStr1
testTCStr2 = HUnit.TestCase $ assertEqual labelTCStr2 expTCStr2 argTCStr2

-- | Check if expressions are correctly categorized as either a
--   declaration or a statement.
testEvalCatD1 = HUnit.TestCase $ assertEqual labelEvalCatD1 expEvalCatDn argEvalCatD1
testEvalCatD2 = HUnit.TestCase $ assertEqual labelEvalCatD2 expEvalCatDn argEvalCatD2
testEvalCatD3 = HUnit.TestCase $ assertEqual labelEvalCatD3 expEvalCatDn argEvalCatD3
testEvalCatD4 = HUnit.TestCase $ assertEqual labelEvalCatD4 expEvalCatDn argEvalCatD4
testEvalCatD5 = HUnit.TestCase $ assertEqual labelEvalCatD5 expEvalCatDn argEvalCatD5
testEvalCatD6 = HUnit.TestCase $ assertEqual labelEvalCatD6 expEvalCatDn argEvalCatD6
testEvalCatS1 = HUnit.TestCase $ assertEqual labelEvalCatS1 expEvalCatSn argEvalCatS1
testEvalCatS2 = HUnit.TestCase $ assertEqual labelEvalCatS2 expEvalCatSn argEvalCatS2
testEvalCatS3 = HUnit.TestCase $ assertEqual labelEvalCatS3 expEvalCatSn argEvalCatS3
testEvalCatS4 = HUnit.TestCase $ assertEqual labelEvalCatS4 expEvalCatSn argEvalCatS4
testEvalCatS5 = HUnit.TestCase $ assertEqual labelEvalCatS5 expEvalCatSn argEvalCatS5

-- | Check whether Daison queries are correctly categorized as such.
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
    HUnit.TestLabel labelEvalRunDecl1 testEvalRunDecl1,
    HUnit.TestLabel labelEvalRunDecl2 testEvalRunDecl2,
    HUnit.TestLabel labelEvalRunDecl3 testEvalRunDecl3,
    HUnit.TestLabel labelEvalRunDecl4 testEvalRunDecl4,
    HUnit.TestLabel labelEvalRunExpr testEvalRunExpr,
    HUnit.TestLabel labelEvalRunStmt testEvalRunStmt,
    HUnit.TestLabel labelEvalCatD1 testEvalCatD1,
    HUnit.TestLabel labelEvalCatD2 testEvalCatD2,
    HUnit.TestLabel labelEvalCatD3 testEvalCatD3,
    HUnit.TestLabel labelEvalCatD4 testEvalCatD4,
    HUnit.TestLabel labelEvalCatD5 testEvalCatD5,
    HUnit.TestLabel labelEvalCatD6 testEvalCatD6,
    HUnit.TestLabel labelEvalCatS1 testEvalCatS1,
    HUnit.TestLabel labelEvalCatS2 testEvalCatS2,
    HUnit.TestLabel labelEvalCatS3 testEvalCatS3,
    HUnit.TestLabel labelEvalCatS4 testEvalCatS4,
    HUnit.TestLabel labelEvalCatS5 testEvalCatS5,
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