{-# Language OverloadedStrings #-}
{-# Language GeneralizedNewtypeDeriving #-}

module EVM.Types where

import Data.Aeson ((.:))
import Data.Aeson (FromJSON (..), FromJSONKey (..), FromJSONKeyFunction (..))
import Data.ByteString (ByteString)
import Data.ByteString.Base16 as BS16
import Data.DoubleWord
import Numeric (readHex, showHex)
import Options.Generic

import qualified Data.Aeson          as JSON
import qualified Data.Aeson.Types    as JSON
import qualified Data.Text           as Text
import qualified Data.Text.Encoding  as Text

newtype Hexword = Hexword { hexWord256 :: Word256 }
  deriving (Num, Integral, Real, Ord, Enum, Eq)

newtype Address = Address { addressWord160 :: Word160 }
  deriving (Num, Integral, Real, Ord, Enum, Eq)

instance Read Hexword where readsPrec _ = readHex . drop 2
instance Show Hexword where showsPrec _ s = ("0x" ++) . showHex s

instance Read Address where readsPrec _ = readHex
instance Show Address where showsPrec _ = showHex

instance FromJSON Hexword where
  parseJSON v = do
    s <- Text.unpack <$> parseJSON v
    case reads s of
      [(x, "")]  -> return (Hexword x)
      [(0, "x")] -> return (Hexword 0)
      _          -> fail $ "invalid hex word (" ++ s ++ ")"

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
  fromJSONKey = FromJSONKeyTextParser $ \s ->
    case reads (Text.unpack s) of
      [(x, "")]  -> return x
      [(0, "x")] -> return 0
      _          -> fail $ "invalid word (" ++ Text.unpack s ++ ")"

instance FromJSONKey Address where
  fromJSONKey = FromJSONKeyTextParser $ \s ->
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
                  <$> (x .: f)

addrField :: JSON.Object -> Text -> JSON.Parser Word160
addrField x f = (readN . ("0x" ++) . Text.unpack)
                  <$> (x .: f)

dataField :: JSON.Object -> Text -> JSON.Parser ByteString
dataField x f = hexText <$> (x .: f)
