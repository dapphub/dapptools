{-# Language LambdaCase #-}
{-# Language NamedFieldPuns #-}
{-# Language OverloadedStrings #-}
{-# Language PatternSynonyms #-}
{-# Language RecordWildCards #-}
{-# Language ViewPatterns #-}

import Restless.Git

import Test.Tasty
import Test.Tasty.HUnit

import Data.ByteString        (ByteString)
import Data.Set               (toList)
import System.IO.Temp         (withSystemTempDirectory)
import Text.Read              (readMaybe)

import qualified Data.ByteString.Char8  as C8
import qualified Data.Set               as Set

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "roundtrip"
  [ testCase "simple example" roundtripExample ]

data Example = Example
  { foo :: Int
  , bar :: Int
  } deriving (Eq, Show)

readBytesMaybe :: Read a => ByteString -> Maybe a
readBytesMaybe = readMaybe . C8.unpack

pattern Reads :: Read a => a -> ByteString
pattern Reads x <- (readBytesMaybe -> Just x)

class Filing a where
  fromFiles :: [File] -> a
  toFiles   :: a -> [File]

instance Filing Example where
  fromFiles = foldl f (Example 0 0)
    where
      f :: Example -> File -> Example
      f x = \case
        File (Path ["example", "foo"] "x") (Reads foo) ->
          x { foo }
        File (Path ["example", "bar"] "x") (Reads bar) ->
          x { bar }
        _ ->
          x
  toFiles Example {..} =
    [ File (Path ["example", "foo"] "x") (C8.pack (show foo))
    , File (Path ["example", "bar"] "x") (C8.pack (show bar))
    ]

roundtripExample :: Assertion
roundtripExample = do
  let x = Example 5 7
  y <- withSystemTempDirectory "restless-git-test" $ \path -> do
    make path
    save path "hello" (Set.fromList (toFiles x))
    (fromFiles . toList) <$> load path
  assertEqual "roundtrip failed" x y
