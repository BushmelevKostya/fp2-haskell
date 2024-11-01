module Main (main) where

import Test.HUnit
import Lib

tests :: Test
tests = TestList [
    TestCase (assertEqual "Test case 1" (1 :: Int) (2 :: Int))
  ]

main :: IO ()
main = runTestTT tests >> return ()
