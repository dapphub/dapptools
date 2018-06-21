{-# Language OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit

import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)

import qualified Data.Text as Text

import Jays (jays)

main :: IO ()
main = defaultMain tests

good :: ByteString -> Text -> ByteString -> Assertion
good a b c = jays a (Text.words b) @?= (c, True)

tests :: TestTree
tests = testGroup "jays"
  [ testCase "Create an object" $ do
      good "" "-n {}" "{}"
      good "" "-n object" "{}"
  , testCase "Create a number" $
      good "" "-n 3" "3"
  , testCase "Create a float" $
      good "" "-n 3.14" "3.14"
  , testCase "Create a boolean" $ do
      good "" "-n true" "true"
      good "" "-n t" "true"
      good "" "-n false" "false"
      good "" "-n f" "false"
  , testCase "Create null" $ do
      good "" "-n null" "null"
      good "" "-n n" "null"
  , testCase "Create array" $ do
      good "" "-n array" "[]"
      good "" "-n []" "[]"

  , testCase "Insert into object" $ do
      good "" "-n {} -n 1 -i a" "{\"a\":1}"
      good "{\"a\":1}" "-n 2 -i b" "{\"a\":1,\"b\":2}"
  , testCase "Append to array" $ do
      good "" "-n [] -n 1 -i append" "[1]"
      good "[0,1]" "-n 2 -i append" "[0,1,2]"

  , testCase "Extract from object" $ do
      good "" "-n {} -n 1 -i a -e a" "1"
  , testCase "Extract from array" $ do
      good "[1,2,3]" "-e 1" "2"

  , testCase "Get type" $ do
      good "1" "-t" "number"
      good "\"x\"" "-t" "string"
      good "true" "-t" "bool"
      good "[]" "-t" "array"
      good "{}" "-t" "object"
      good "null" "-t" "null"

  , testCase "Print keys" $ do
      good "" "-n {} -n 0 -i a -n 1 -i b -k" "a\nb"

  , testCase "String" $ do
      good "" "-s hey" "\"hey\""
  , testCase "Unstring" $ do
      good "" "-s hey -u" "hey"
      good "" "-n 3 -u" "3"
      good "" "-n 3.14159 -u" "3.14159"
      good "" "-n null -u" "null"
      good "" "-n true -u" "true"

  , testCase "Across" $ do
      good "[1,null]" "-a -t" "number\nnull"

  , testCase "Length" $ do
      good "[1,2]" "-n 3 -i append -l" "3"

  , testCase "Errors" $ do
      jays "" ["-n", "2", "-i", "foo"] @?= ("jays: error in -i", False)
      jays "" ["-Q", "-n", "2", "-i", "foo"] @?= ("", False)
  ]

-- Summary of jshon keywords:
--
--     -Q  (quiet)      no error output
--     -a  (across)     maps remaining actions
--     -e  (extract)    index into objects/arrays
--     -i  (insert)     inserts into objects/arrays
--     -j  (literal)    prints encoded JSON
--     -k  (keys)       prints keys
--     -n  (nonstring)  makes a JSON element
--     -s  (string)     makes a JSON string
--     -u  (unstring)   prints unquoted string or simple type
