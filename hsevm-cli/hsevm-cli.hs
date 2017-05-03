{-# Language BangPatterns #-}
{-# Language DeriveGeneric #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language TemplateHaskell #-}

import qualified EVM as EVM
import EVM.Keccak
import EVM.Solidity

import Data.ByteString as BS
import Data.ByteString.Base16 as BS16
import Data.DoubleWord
import Numeric
import Options.Generic
import System.Exit

newtype Hexword = Hexword { hexWord256 :: Word256 }
  deriving (Num, Integral, Real, Ord, Enum, Eq)

instance Read Hexword where readsPrec _ = readHex
instance Show Hexword where showsPrec _ = showHex

instance ParseField Hexword
instance ParseFields Hexword
instance ParseRecord Hexword where
  parseRecord = fmap getOnly parseRecord

data Command
  = Exec
      { code :: BS.ByteString
      , trace :: Bool
      , calldata :: Maybe BS.ByteString
      , value :: Maybe Hexword
      , address :: Maybe Hexword
      , caller :: Maybe Hexword
      , origin :: Maybe Hexword
      , number :: Maybe Hexword
      , timestamp :: Maybe Hexword
      , coinbase :: Maybe Hexword
      , gaslimit :: Maybe Hexword
      , difficulty :: Maybe Hexword
      }
  deriving (Show, Generic, Eq)

instance ParseRecord Command

hexByteString msg bs =
  case BS16.decode bs of
    (x, "") -> x
    _ -> error ("invalid hex bytestring for " ++ msg)

main :: IO ()
main = do
  opts <- getRecord "hsevm -- Ethereum evaluator"
  case opts of
    Exec _ _ _ _ _ _ _ _ _ _ _ _ ->
      let word f def = maybe def hexWord256 (f opts)
      in print . EVM.makeVm $ EVM.VMOpts
        { EVM.vmoptCode       = hexByteString "--code" (code opts)
        , EVM.vmoptCalldata   = maybe "" (hexByteString "--calldata") (calldata opts)
        , EVM.vmoptValue      = word value 0
        , EVM.vmoptAddress    = word address 1
        , EVM.vmoptCaller     = word caller 2
        , EVM.vmoptOrigin     = word origin 3
        , EVM.vmoptNumber     = word number 0
        , EVM.vmoptTimestamp  = word timestamp 0
        , EVM.vmoptCoinbase   = word coinbase 0
        , EVM.vmoptGaslimit   = word gaslimit 0
        , EVM.vmoptDifficulty = word difficulty 0
        }
