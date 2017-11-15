module Main where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

tests :: TestTree
tests = testGroup
  "tests" [
    testCase "true"
      $ assertEqual "true" True True
  ]

main :: IO ()
main = defaultMain tests
