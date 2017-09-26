{-# Language OverloadedStrings #-}
{-# Language QuasiQuotes #-}

import Data.Text (Text)
import Data.ByteString (ByteString)

import qualified Data.Text as Text
import qualified Data.ByteString as BS

import Test.Tasty
import Test.Tasty.QuickCheck (testProperty, Arbitrary (..), NonNegative (..))
import Test.Tasty.HUnit

import Control.Monad.State.Strict (execState, runState)
import Control.Lens

import Data.Monoid
import qualified Data.Vector as Vector
import Data.String.Here

import Data.Binary.Put (runPut)
import Data.Binary.Get (runGetOrFail)

import EVM
import EVM.ABI
import EVM.Concrete
import EVM.Exec
import EVM.Solidity
import EVM.Types

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
        SolidityCall "x = uint(keccak256(a));"
          [AbiString ""] ===> AbiUInt 256 0xc5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470
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

singleContract :: Text -> Text -> IO (Maybe ByteString)
singleContract x s =
  solidity x [i|
    contract ${x} { ${s} }
  |]

runStatements
  :: Text -> [AbiValue] -> AbiType
  -> IO (Maybe ByteString)
runStatements stmts args t = do
  let params =
        Text.intercalate ", "
          (map (\(x, c) -> abiTypeSolidity (abiValueType x)
                             <> " " <> Text.pack [c])
            (zip args "abcdefg"))
      sig =
        "foo(" <> Text.intercalate ","
                    (map (abiTypeSolidity . abiValueType) args) <> ")"

  Just x <- singleContract "X" [i|
    function foo(${params}) returns (${abiTypeSolidity t} x) {
      ${stmts}
    }
  |]

  case runState exec (vmForEthrunCreation x) of
    (VMSuccess (B targetCode), vm1) -> do
      let target = view (state . contract) vm1
          vm2 = execState (replaceCodeOfSelf targetCode) vm1
      case flip runState vm2
             (do resetState
                 assign (state . gas) 0xffffffffffffffff -- kludge
                 loadContract target
                 assign (state . calldata)
                   (B (abiCalldata sig (Vector.fromList args)))
                 exec) of
        (VMSuccess (B out), _) ->
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
