{-# Language OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit

import EVM.Precompiled

import Data.ByteString (ByteString)
import Data.Monoid ((<>))

import qualified Data.ByteString.Base16 as Hex

main :: IO ()
main =
  defaultMain . testGroup "Precompiled contracts" $
    [ testGroup "Example (reverse)"
        [ testCase "success" $
            assertEqual "example contract reverses"
              (execute 0xdeadbeef "foobar" 6) (Just "raboof")
        , testCase "failure" $
            assertEqual "example contract fails on length mismatch"
              (execute 0xdeadbeef "foobar" 5) Nothing
        ]

    , testGroup "ECRECOVER"
        [ testCase "success" $ do
            let
              r = hex "c84e55cee2032ea541a32bf6749e10c8b9344c92061724c4e751600f886f4732"
              s = hex "1542b6457e91098682138856165381453b3d0acae2470286fd8c8a09914b1b5d"
              v = hex "000000000000000000000000000000000000000000000000000000000000001c"
              h = hex "513954cf30af6638cb8f626bd3f8c39183c26784ce826084d9d267868a18fb31"
              a = hex "0000000000000000000000002d5e56d45c63150d937f2182538a0f18510cb11f"
            assertEqual "successful recovery"
              (Just a)
              (execute 1 (h <> v <> r <> s) 32)
        , testCase "fail on made up values" $ do
            let
              r = hex "c84e55cee2032ea541a32bf6749e10c8b9344c92061724c4e751600f886f4731"
              s = hex "1542b6457e91098682138856165381453b3d0acae2470286fd8c8a09914b1b5d"
              v = hex "000000000000000000000000000000000000000000000000000000000000001c"
              h = hex "513954cf30af6638cb8f626bd3f8c39183c26784ce826084d9d267868a18fb31"
            assertEqual "fail because bit flip"
              Nothing
              (execute 1 (h <> v <> r <> s) 32)
        ]
    ]

hex :: ByteString -> ByteString
hex s =
  case Hex.decode s of
    (x, "") -> x
    _ -> error "internal error"
