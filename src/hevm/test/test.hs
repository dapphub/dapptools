{-# Language OverloadedStrings #-}
{-# Language QuasiQuotes #-}

import Data.Text (Text)
import Data.ByteString (ByteString)

import qualified Data.Text as Text
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Hex

import Test.Tasty
import Test.Tasty.QuickCheck (testProperty, Arbitrary (..), NonNegative (..))
import Test.Tasty.HUnit

import Control.Monad.State.Strict (execState, runState)
import Control.Lens

import qualified Data.Vector as Vector
import Data.String.Here

import Data.Binary.Put (runPut)
import Data.Binary.Get (runGetOrFail)

import EVM
import EVM.ABI
import EVM.Exec
import EVM.Solidity
import EVM.Types
import EVM.Precompiled

main :: IO ()
main = defaultMain $ testGroup "hevm"
  [ testGroup "ABI"
    [ testProperty "Put/get inverse" $ \x ->
        case runGetOrFail (getAbi (abiValueType x)) (runPut (putAbi x)) of
          Right ("", _, x') -> x' == x
          _ -> False
    ]

  , testGroup "Solidity expressions"
    [ testCase "Trivial" $ do
        SolidityCall "x = 3;" []
          ===> AbiUInt 256 3

    , testCase "Arithmetic" $ do
        SolidityCall "x = a + 1;"
          [AbiUInt 256 1] ===> AbiUInt 256 2
        SolidityCall "x = a - 1;"
          [AbiUInt 8 0] ===> AbiUInt 8 255

    , testCase "keccak256()" $ do
        SolidityCall "x = uint(keccak256(abi.encodePacked(a)));"
          [AbiString ""] ===> AbiUInt 256 0xc5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470
    ]

  , testGroup "ecrecover"
    [ testCase "Example 1" $
        let
          h = "0x0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
          r = "0xc84e55cee2032ea541a32bf6749e10c8b9344c92061724c4e751600f886f4732"
          s = "0x1542b6457e91098682138856165381453b3d0acae2470286fd8c8a09914b1b5d"
        in SolidityCall
             (Text.unlines
               [ "bytes32 h = keccak256(abi.encodePacked(\"\\x19Ethereum Signed Message:\\n32\", d));"
               , "x = ecrecover(h, a, b, c);"
               ])
             [ AbiUInt 8 28
             , AbiBytes 32 (hexText r)
             , AbiBytes 32 (hexText s)
             , AbiBytes 32 (hexText h)
             ]
             ===>
             AbiAddress 0x2d5e56d45c63150d937f2182538a0f18510cb11f
    ]

  , testGroup "Precompiled contracts" $
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

  , testGroup "Byte/word manipulations"
    [ testProperty "padLeft length" $ \n (Bytes bs) ->
        BS.length (padLeft n bs) == max n (BS.length bs)
    , testProperty "padLeft identity" $ \(Bytes bs) ->
        padLeft (BS.length bs) bs == bs
    , testProperty "padRight length" $ \n (Bytes bs) ->
        BS.length (padLeft n bs) == max n (BS.length bs)
    , testProperty "padRight identity" $ \(Bytes bs) ->
        padLeft (BS.length bs) bs == bs
    , testProperty "padLeft zeroing" $ \(NonNegative n) (Bytes bs) ->
        let x = BS.take n (padLeft (BS.length bs + n) bs)
            y = BS.replicate n 0
        in x == y
    ]
  ]

  where
    (===>) = assertSolidityComputation

hex :: ByteString -> ByteString
hex s =
  case Hex.decode s of
    (x, "") -> x
    _ -> error "internal error"

singleContract :: Text -> Text -> IO (Maybe ByteString)
singleContract x s =
  solidity x [i|
    contract ${x} { ${s} }
  |]

defaultDataLocation :: AbiType -> Text
defaultDataLocation t =
  if (case t of
        AbiBytesDynamicType -> True
        AbiStringType -> True
        AbiArrayDynamicType _ -> True
        AbiArrayType _ _ -> True
        _ -> False)
  then "memory"
  else ""

runStatements
  :: Text -> [AbiValue] -> AbiType
  -> IO (Maybe ByteString)
runStatements stmts args t = do
  let params =
        Text.intercalate ", "
          (map (\(x, c) -> abiTypeSolidity (abiValueType x)
                             <> " " <> defaultDataLocation (abiValueType x)
                             <> " " <> Text.pack [c])
            (zip args "abcdefg"))
      sig =
        "foo(" <> Text.intercalate ","
                    (map (abiTypeSolidity . abiValueType) args) <> ")"

  Just x <- singleContract "X" [i|
    function foo(${params}) public pure returns (${abiTypeSolidity t} x) {
      ${stmts}
    }
  |]

  case runState exec (vmForEthrunCreation x) of
    (VMSuccess targetCode, vm1) -> do
      let target = view (state . contract) vm1
          vm2 = execState (replaceCodeOfSelf (RuntimeCode targetCode)) vm1
      case flip runState vm2
             (do resetState
                 assign (state . gas) 0xffffffffffffffff -- kludge
                 loadContract target
                 assign (state . calldata)
                   (abiCalldata sig (Vector.fromList args))
                 exec) of
        (VMSuccess out, _) ->
          return (Just out)
        (VMFailure problem, _) -> do
          print problem
          return Nothing
    _ ->
      return Nothing

newtype Bytes = Bytes ByteString
  deriving Eq

instance Show Bytes where
  showsPrec _ (Bytes x) _ = show (BS.unpack x)

instance Arbitrary Bytes where
  arbitrary = fmap (Bytes . BS.pack) arbitrary

data Invocation
  = SolidityCall Text [AbiValue]
  deriving Show

assertSolidityComputation :: Invocation -> AbiValue -> IO ()
assertSolidityComputation (SolidityCall s args) x =
  do y <- runStatements s args (abiValueType x)
     assertEqual (Text.unpack s)
       (fmap Bytes (Just (encodeAbiValue x)))
       (fmap Bytes y)
