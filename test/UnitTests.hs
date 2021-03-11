{-# LANGUAGE MonadComprehensions #-}
module Main (main) where

import Test.QuickCheck
import Test.HUnit ((@?=), (@?), assertFailure, assertEqual, assertBool)
import qualified Test.HUnit as HUnit

import System.Exit

foo x = (1,x-1)
test1 = HUnit.TestCase (assertEqual "for (foo 3)," (1,2) (foo 3))
test2 = HUnit.TestCase (assertBool "true test," True)

main :: IO ()
main = do
    c1 <- runUnitTests unitTestList
    let failures = HUnit.errors c1 + HUnit.failures c1
        exit_code
            | failures > 0 = ExitFailure failures
            | otherwise    = ExitSuccess
    exitWith exit_code

unitTestList :: HUnit.Test
unitTestList = HUnit.TestList [HUnit.TestLabel "test1" test1, HUnit.TestLabel "test2" test2]

runUnitTests :: HUnit.Test -> IO HUnit.Counts
runUnitTests = HUnit.runTestTT 