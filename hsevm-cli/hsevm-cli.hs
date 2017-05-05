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

import qualified Data.ByteString.Lazy as Lazy

import IPPrint.Colored (cpprint)

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import Data.DoubleWord
import Data.Map (Map)
import Numeric
import Options.Generic
import System.Exit

import qualified Data.HashMap.Lazy as HashMap

import Data.Aeson (parseJSON, FromJSON, FromJSONKey)
import Data.Aeson ((.:), (.:?), (.!=))

import Control.Lens
import Data.Aeson.Lens

import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.Map as Map

newtype Hexword = Hexword { hexWord256 :: Word256 }
  deriving (Num, Integral, Real, Ord, Enum, Eq)

newtype Address = Address { addressWord256 :: Word256 }
  deriving (Num, Integral, Real, Ord, Enum, Eq)

instance Read Hexword where readsPrec _ = readHex . drop 2
instance Show Hexword where showsPrec _ s = ("0x" ++) . showHex s

instance Read Address where readsPrec _ = readHex
instance Show Address where showsPrec _ = showHex

instance FromJSON Hexword where
  parseJSON v = do
    s <- Text.unpack <$> parseJSON v
    case reads s of
      [(x, "")] -> return x
      _         -> fail $ "invalid hex word (" ++ s ++ ")"

instance FromJSON Word256 where
  parseJSON v =
    read . Text.unpack <$> parseJSON v

instance FromJSON Address where
  parseJSON v = do
    s <- Text.unpack <$> parseJSON v
    case reads s of
      [(x, "")] -> return x
      _         -> fail $ "invalid address (" ++ s ++ ")"

instance FromJSONKey Word256 where
  fromJSONKey = JSON.FromJSONKeyTextParser $ \s ->
    case reads (Text.unpack s) of
      [(x, "")] -> return x
      _         -> fail $ "invalid word (" ++ Text.unpack s ++ ")"

instance FromJSONKey Address where
  fromJSONKey = JSON.FromJSONKeyTextParser $ \s ->
    case reads (Text.unpack s) of
      [(x, "")] -> return x
      _         -> fail $ "invalid word (" ++ Text.unpack s ++ ")"

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
  , testContracts   :: Map Address ContractSpec
  , testExpectation :: VMTestExpectation
  } deriving Show

data ContractSpec = ContractSpec
  { contractBalance :: Hexword
  , contractCode    :: ByteString
  , contractNonce   :: Hexword
  , contractStorage :: Map Word256 Word256
  } deriving Show

data VMTestExpectation = VMTestExpectation
  { expectedOut :: Maybe ByteString
  , expectedContracts :: Maybe (Map Address ContractSpec)
  } deriving Show

instance FromJSON ContractSpec where
  parseJSON (JSON.Object v) = ContractSpec
    <$> v .: "balance"
    <*> (hexText <$> v .: "code")
    <*> v .: "nonce"
    <*> v .: "storage"

instance FromJSON VMTestCase where
  parseJSON (JSON.Object v) = VMTestCase
    <$> parseTestVmOpts v
    <*> parseTestContracts v
    <*> parseTestExpectation v
  parseJSON invalid =
      JSON.typeMismatch "VM test case" invalid

parseTestVmOpts :: JSON.Object -> JSON.Parser EVM.VMOpts
parseTestVmOpts v =
  do envV  <- v .: "env"
     execV <- v .: "exec"
     case (envV, execV) of
       (JSON.Object env, JSON.Object exec) ->
         EVM.VMOpts
           <$> dataField exec "code"
           <*> dataField exec "data"
           <*> wordField exec "value"
           <*> addrField exec "address"
           <*> addrField exec "caller"
           <*> addrField exec "origin"
           <*> addrField env  "currentCoinbase"
           <*> wordField env  "currentNumber"
           <*> wordField env  "currentTimestamp"
           <*> wordField env  "currentGasLimit"
           <*> wordField env  "currentDifficulty"
       _ ->
         JSON.typeMismatch "VM test case" (JSON.Object v)

parseTestContracts ::
  JSON.Object -> JSON.Parser (Map Address ContractSpec)
parseTestContracts v =
  v .: "pre" >>= parseJSON

parseTestExpectation :: JSON.Object -> JSON.Parser VMTestExpectation
parseTestExpectation v =
  VMTestExpectation
    <$> (fmap hexText <$> v .:? "out")
    <*> v .:? "post"

parseTestCases ::
  Lazy.ByteString -> Either String (Map String VMTestCase)
parseTestCases = JSON.eitherDecode'

hexByteString :: String -> ByteString -> ByteString
hexByteString msg bs =
  case BS16.decode bs of
    (x, "") -> x
    _ -> error ("invalid hex bytestring for " ++ msg)

hexText :: Text -> ByteString
hexText t =
  case BS16.decode (Text.encodeUtf8 (Text.drop 2 t)) of
    (x, "") -> x
    _ -> error ("invalid hex bytestring " ++ show t)

readN :: Integral a => String -> a
readN s = fromIntegral (read s :: Integer)

wordField :: JSON.Object -> Text -> JSON.Parser Word256
wordField x f = (readN . Text.unpack)
                  <$> (x .: f >>= parseJSON)

addrField :: JSON.Object -> Text -> JSON.Parser Word256
addrField x f = (readN . ("0x" ++) . Text.unpack)
                  <$> (x .: f >>= parseJSON)

dataField :: JSON.Object -> Text -> JSON.Parser ByteString
dataField x f = hexText . Text.drop 2
                  <$> (x .: f >>= parseJSON)

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
    VMTest f -> Lazy.readFile f >>= cpprint . parseTestCases
