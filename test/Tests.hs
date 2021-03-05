module Main (main) where

import Test.QuickCheck
 
prop_dummyTest :: [Char] -> Bool
prop_dummyTest s = (reverse . reverse) s == s
 
main = do
  quickCheck prop_dummyTest                                             
