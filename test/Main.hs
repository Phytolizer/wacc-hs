module Main (main) where

import Test.HUnit (Test (TestLabel, TestList), runTestTTAndExit)

import Spec.Document (docTests)

allTests :: Test
allTests = TestList [TestLabel "Document" docTests]

main :: IO ()
main = runTestTTAndExit allTests
