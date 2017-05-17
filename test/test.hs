{-# Language OverloadedStrings #-}
{-# Language QuasiQuotes #-}

import Data.Text (Text)
import Data.ByteString (ByteString)

import qualified Data.Text as Text
import qualified Data.ByteString as ByteString

import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)
import Test.Tasty.HUnit

import Control.Monad.State (execState, runState)
import Control.Lens

import Data.Monoid

import Data.String.Here

import EVM
import EVM.ABI
import EVM.Keccak
import EVM.Solidity
import EVM.Exec
import EVM.Debug

import IPPrint.Colored (cpprint)

singleContract :: Text -> Text -> IO (Maybe ByteString)
singleContract x s =
  solidity x [i|
    contract ${x} { ${s} }
  |]

runStatements :: Text -> Text -> IO (Maybe ByteString)
runStatements stmts t = do
  Just x <- singleContract "X" [i|
    function x() returns (${t} x) {
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
                 assign (state . calldata) (word32Bytes (abiKeccak "x()"))
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

assertSolidityComputation x s =
  do y <- runStatements s (abiTypeSolidity (abiValueType x))
     assertEqual (Text.unpack s)
       (fmap Bytes (Just (encodeAbiValue x)))
       (fmap Bytes y)

(===>) = flip assertSolidityComputation

main :: IO ()
main = defaultMain $
  testGroup "Solidity expressions"
    [ testCase "Arithmetic" $
        "x = 3;" ===> AbiUInt 256 3
    ]
