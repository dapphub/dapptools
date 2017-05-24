{-# Language CPP #-}
{-# Language TemplateHaskell #-}

module EVM.Types where

import Control.DeepSeq
import Data.Aeson ((.:))
import Data.Aeson (FromJSON (..))

#if MIN_VERSION_aeson(1, 0, 0)
import Data.Aeson (FromJSONKey (..), FromJSONKeyFunction (..))
#endif

import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString.Base16 as BS16
import Data.DoubleWord
import Data.DoubleWord.TH
import Numeric (readHex, showHex)
import Options.Generic

import qualified Data.Aeson          as JSON
import qualified Data.Aeson.Types    as JSON
import qualified Data.Text           as Text
import qualified Data.Text.Encoding  as Text

-- Some stuff for "generic programming", needed to create Word512
import Data.Data

-- We need a 512-bit word for doing ADDMOD and MULMOD with full precision.
mkUnpackedDoubleWord "Word512" ''Word256 "Int512" ''Int256 ''Word256
  [''Typeable, ''Data, ''Generic]

newtype W256 = W256 Word256
  deriving (Num, Integral, Real, Ord, Enum, Eq, Bits, Generic)

newtype Addr = Addr { addressWord160 :: Word160 }
  deriving (Num, Integral, Real, Ord, Enum, Eq, Bits, Generic)

instance NFData Word128
instance NFData Word256
instance NFData Int128
instance NFData Int256
instance NFData W256
instance NFData Word160
instance NFData Addr

instance Read W256 where readsPrec n s = (\(x, r) -> (W256 x, r)) <$> readsPrec n s
instance Show W256 where showsPrec _ s = ("0x" ++) . showHex s

instance Read Addr where readsPrec _ s = readHex s
instance Show Addr where showsPrec _ s = showHex s

instance FromJSON W256 where
  parseJSON v = do
    s <- Text.unpack <$> parseJSON v
    case reads s of
      [(x, "")]  -> return (W256 x)
      [(0, "x")] -> return (W256 0)
      _          -> fail $ "invalid hex word (" ++ s ++ ")"

-- instance FromJSON W256 where
--   parseJSON v =
--     read . Text.unpack <$> parseJSON v

instance FromJSON Addr where
  parseJSON v = do
    s <- Text.unpack <$> parseJSON v
    case reads s of
      [(x, "")] -> return x
      _         -> fail $ "invalid address (" ++ s ++ ")"

#if MIN_VERSION_aeson(1, 0, 0)

instance FromJSONKey W256 where
  fromJSONKey = FromJSONKeyTextParser $ \s ->
    case reads (Text.unpack s) of
      [(x, "")]  -> return x
      [(0, "x")] -> return 0
      _          -> fail $ "invalid word (" ++ Text.unpack s ++ ")"

instance FromJSONKey Addr where
  fromJSONKey = FromJSONKeyTextParser $ \s ->
    case reads (Text.unpack s) of
      [(x, "")] -> return x
      _         -> fail $ "invalid word (" ++ Text.unpack s ++ ")"

#endif

instance ParseField W256
instance ParseFields W256
instance ParseRecord W256 where
  parseRecord = fmap getOnly parseRecord

instance ParseField Addr
instance ParseFields Addr
instance ParseRecord Addr where
  parseRecord = fmap getOnly parseRecord

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

wordField :: JSON.Object -> Text -> JSON.Parser W256
wordField x f = (readN . Text.unpack)
                  <$> (x .: f)

addrField :: JSON.Object -> Text -> JSON.Parser Addr
addrField x f = (readN . ("0x" ++) . Text.unpack)
                  <$> (x .: f)

dataField :: JSON.Object -> Text -> JSON.Parser ByteString
dataField x f = hexText <$> (x .: f)
