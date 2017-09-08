{-# Language LambdaCase #-}
{-# Language NamedFieldPuns #-}
{-# Language OverloadedStrings #-}
{-# Language PatternSynonyms #-}
{-# Language RecordWildCards #-}
{-# Language ViewPatterns #-}

import Restless.Git

import Control.Monad.IO.Class (liftIO)
import Data.ByteString        (ByteString)
import Data.Set               (toList)
import System.IO.Temp         (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.Hedgehog
import Text.Read              (readMaybe)

import Hedgehog
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

import qualified Data.ByteString.Char8  as C8
import qualified Data.Set               as Set

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "roundtrip"
  [ testProperty "simple example" prop_roundtrip_example ]

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
        File (Path ["example"] "foo") (Reads foo) ->
          x { foo }
        File (Path ["example"] "bar") (Reads bar) ->
          x { bar }
        _ ->
          x
  toFiles Example {..} =
    [ File (Path ["example"] "foo") (C8.pack (show foo))
    , File (Path ["example"] "bar") (C8.pack (show bar))
    ]

prop_roundtrip_example :: Property
prop_roundtrip_example = property $ do
  x <- forAll $
    Example
      <$> Gen.integral (Range.linear 0 100)
      <*> Gen.integral (Range.linear 0 100)
  y <- liftIO $ withSystemTempDirectory "restless-git-test" $ \path -> do
    metadata <- Metadata "x" "y" "z" <$> now
    make path
    save path metadata (Set.fromList (toFiles x))
    (fromFiles . toList) <$> load path
  assert (x == y)
