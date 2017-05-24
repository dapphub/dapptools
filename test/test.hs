{-# Language OverloadedStrings #-}
{-# Language QuasiQuotes #-}

import Data.Text (Text)
import Data.ByteString (ByteString)

import qualified Data.Text as Text
import qualified Data.ByteString as ByteString

import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.HUnit

import Control.Monad.State.Strict (execState, runState)
import Control.Lens

import Data.Monoid
import qualified Data.Vector as Vector
import Data.String.Here

import Data.Binary.Put (runPut)
import Data.Binary.Get (runGetState)

import qualified Data.ByteString.Lazy as BSLazy

import EVM
import EVM.ABI
import EVM.Keccak
import EVM.Solidity
import EVM.Exec
import EVM.Debug

import IPPrint.Colored (cpprint)

main :: IO ()
main = defaultMain $ testGroup "hsevm"
  [ testGroup "ABI"
    [ testProperty "Put/get inverse" $ \x ->
        let bytes = runPut (putAbi x)
            (x', remaining, _) = runGetState (getAbi (abiValueType x)) bytes 0
        in x == x' && BSLazy.null remaining
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
        "x(" <> Text.intercalate ","
                  (map (abiTypeSolidity . abiValueType) args) <> ")"

  Just x <- singleContract "X" [i|
    function x(${params}) returns (${abiTypeSolidity t} x) {
      ${stmts}
    }
  |]

  case runState exec (vmForEthrunCreation x) of
    (VMSuccess targetCode, vm1) -> do
      let target = view (state . contract) vm1
          vm2 = execState (performCreation targetCode) vm1
      case flip runState vm2
             (do resetState
                 loadContract target
                 assign (state . calldata)
                   (abiCalldata sig (Vector.fromList args))
                 exec) of
        (VMSuccess out, _) ->
          return (Just out)
        _ ->
          return Nothing
    _ ->
      return Nothing

newtype Bytes = Bytes ByteString
  deriving Eq

instance Show Bytes where
  showsPrec _ (Bytes x) _ = show (ByteString.unpack x)

data Invocation
  = SolidityCall Text [AbiValue]
  deriving Show

assertSolidityComputation (SolidityCall s args) x =
  do y <- runStatements s args (abiValueType x)
     assertEqual (Text.unpack s)
       (fmap Bytes (Just (encodeAbiValue x)))
       (fmap Bytes y)
