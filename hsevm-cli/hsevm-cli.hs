{-# Language BangPatterns #-}
{-# Language DeriveGeneric #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language TemplateHaskell #-}

import qualified EVM as EVM
import EVM.Keccak
import EVM.Solidity

import Data.ByteString (ByteString)
import Data.ByteString.Base16 as BS16
import Data.DoubleWord
import Data.Map (Map)
import Numeric
import Options.Generic
import System.Exit

import qualified Data.Aeson as JSON
import qualified Data.Map as Map

newtype Hexword = Hexword { hexWord256 :: Word256 }
  deriving (Num, Integral, Real, Ord, Enum, Eq)

newtype Address = Address { addressWord256 :: Word256 }
  deriving (Num, Integral, Real, Ord, Enum, Eq)

instance Read Hexword where readsPrec _ = readHex . drop 2
instance Show Hexword where showsPrec _ s = ("0x" ++) . showHex s

instance Read Address where readsPrec _ = readHex
instance Show Address where showsPrec _ = showHex

instance ParseField Hexword
instance ParseFields Hexword
instance ParseRecord Hexword where
  parseRecord = fmap getOnly parseRecord

instance ParseField Address
instance ParseFields Address
instance ParseRecord Address where
  parseRecord = fmap getOnly parseRecord

data Command
  = Exec
      { code       :: ByteString
      , trace      :: Bool
      , calldata   :: Maybe ByteString
      , address    :: Maybe Address
      , caller     :: Maybe Address
      , origin     :: Maybe Address
      , coinbase   :: Maybe Address
      , value      :: Maybe Hexword
      , number     :: Maybe Hexword
      , timestamp  :: Maybe Hexword
      , gaslimit   :: Maybe Hexword
      , difficulty :: Maybe Hexword
      }
  | VMTest
      { file :: String }
  deriving (Show, Generic, Eq)

instance ParseRecord Command

data VMTestCase = VMTestCase
  { testVmOpts      :: EVM.VMOpts
  , testContracts   :: Map Word256 ContractSpec
  , testExpectation :: VMTestCaseExpectation
  } deriving Show

data ContractSpec = ContractSpec
  { contractBalance :: Hexword
  , contractCode    :: ByteString
  , contractNonce   :: Hexword
  , contractStorage :: Map Word256 Word256
  } deriving Show

data VMTestCaseExpectation = VMTestCaseExpectation
  { expectedOut :: ByteString
  , expectedContracts :: Map Address ContractSpec
  } deriving Show

hexByteString msg bs =
  case BS16.decode bs of
    (x, "") -> x
    _ -> error ("invalid hex bytestring for " ++ msg)

vmFromCommand :: Command -> EVM.VM
vmFromCommand opts =
  EVM.makeVm $ EVM.VMOpts
    { EVM.vmoptCode       = hexByteString "--code" (code opts)
    , EVM.vmoptCalldata   = maybe "" (hexByteString "--calldata")
                              (calldata opts)
    , EVM.vmoptValue      = word value 0
    , EVM.vmoptAddress    = addr address 1
    , EVM.vmoptCaller     = addr caller 2
    , EVM.vmoptOrigin     = addr origin 3
    , EVM.vmoptCoinbase   = addr coinbase 0
    , EVM.vmoptNumber     = word number 0
    , EVM.vmoptTimestamp  = word timestamp 0
    , EVM.vmoptGaslimit   = word gaslimit 0
    , EVM.vmoptDifficulty = word difficulty 0
    }
  where
    word f def = maybe def hexWord256 (f opts)
    addr f def = maybe def addressWord256 (f opts)

main :: IO ()
main = do
  opts <- getRecord "hsevm -- Ethereum evaluator"
  case opts of
    Exec {}  -> print (vmFromCommand opts)
    VMTest f -> print f
