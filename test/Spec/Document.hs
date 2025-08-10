module Spec.Document (docTests) where

import Test.HUnit (Test (TestCase, TestLabel, TestList), assertEqual)

import qualified Data.Text as T

import WaCC.Document (docLineIndex, parseDocument)

sampleDoc :: T.Text
sampleDoc = T.pack "Hello, World!\nThis is a test document.\nIt has multiple lines.\n"

sampleDocNoEol :: T.Text
sampleDocNoEol = T.pack "Hello, World!\nThis is a test document.\n It has multiple lines."

testLineIndex :: T.Text -> Test
testLineIndex text = TestCase $ do
  let doc = parseDocument "test.txt" text
  assertEqual "First line at offset 0" 0 (docLineIndex doc 0)
  assertEqual "Newline is part of the first line" 0 (docLineIndex doc 13)
  assertEqual "Second line at offset 14" 1 (docLineIndex doc 14)
  assertEqual "Third line at offset 40" 2 (docLineIndex doc 40)
  assertEqual "Last line at offset 64" 3 (docLineIndex doc 64)
  assertEqual "Offset beyond last line" 3 (docLineIndex doc 100)

docTests :: Test
docTests =
  TestList
    [ TestLabel "docLineIndex with final EOL" (testLineIndex sampleDoc)
    , TestLabel "docLineIndex without final EOL" (testLineIndex sampleDocNoEol)
    ]
