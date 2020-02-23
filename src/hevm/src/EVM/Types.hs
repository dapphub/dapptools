{-# Language CPP #-}
{-# Language TemplateHaskell #-}

module EVM.Types where

import Data.Aeson ((.:))
import Data.Aeson (FromJSON (..))

#if MIN_VERSION_aeson(1, 0, 0)
import Data.Aeson (FromJSONKey (..), FromJSONKeyFunction (..))
#endif

import Data.Monoid ((<>))
import Data.Bits
import Data.Char
import Data.ByteString (ByteString)
import Data.ByteString.Base16 as BS16
import Data.ByteString.Builder (byteStringHex, toLazyByteString)
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Char8  as Char8
import Data.DoubleWord
import Data.DoubleWord.TH
import Data.Word (Word8)
import Numeric (readHex, showHex)
import Options.Generic

import qualified Data.Aeson          as JSON
import qualified Data.Aeson.Types    as JSON
import qualified Data.ByteString     as BS
import qualified Data.Serialize.Get  as Cereal
import qualified Data.Text           as Text
import qualified Data.Text.Encoding  as Text
import qualified Text.Read

-- Some stuff for "generic programming", needed to create Word512
import Data.Data

-- We need a 512-bit word for doing ADDMOD and MULMOD with full precision.
mkUnpackedDoubleWord "Word512" ''Word256 "Int512" ''Int256 ''Word256
  [''Typeable, ''Data, ''Generic]

newtype W256 = W256 Word256
  deriving
    ( Num, Integral, Real, Ord, Enum, Eq
    , Bits, FiniteBits, Generic
    )

newtype Addr = Addr { addressWord160 :: Word160 }
  deriving (Num, Integral, Real, Ord, Enum, Eq, Bits, Generic)

instance Read W256 where
  readsPrec _ "0x" = [(0, "")]
  readsPrec n s = (\(x, r) -> (W256 x, r)) <$> readsPrec n s

instance Show W256 where
  showsPrec _ s = ("0x" ++) . showHex s

instance Read Addr where
  readsPrec _ ('0':'x':s) = readHex s
  readsPrec _ s = readHex s

instance Show Addr where
  showsPrec _ s a =
    let h = showHex s a
    in replicate (40 - length h) '0' ++ h

showAddrWith0x :: Addr -> String
showAddrWith0x addr = "0x" ++ show addr

showWordWith0x :: W256 -> String
showWordWith0x addr = show addr

strip0x :: ByteString -> ByteString
strip0x bs = if "0x" `Char8.isPrefixOf` bs then Char8.drop 2 bs else bs

newtype ByteStringS = ByteStringS ByteString

instance Show ByteStringS where
  show (ByteStringS x) = ("0x" ++) . Text.unpack . fromBinary $ x
    where
      fromBinary =
        Text.decodeUtf8 . toStrict . toLazyByteString . byteStringHex

instance FromJSON W256 where
  parseJSON v = do
    s <- Text.unpack <$> parseJSON v
    case reads s of
      [(x, "")]  -> return x
      _          -> fail $ "invalid hex word (" ++ s ++ ")"

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

readNull :: Read a => a -> String -> a
readNull x s = case Text.Read.readMaybe s of
  Just y  -> y
  Nothing -> x

wordField :: JSON.Object -> Text -> JSON.Parser W256
wordField x f = ((readNull 0) . Text.unpack)
                  <$> (x .: f)

addrField :: JSON.Object -> Text -> JSON.Parser Addr
addrField x f = (read . Text.unpack) <$> (x .: f)

addrFieldMaybe :: JSON.Object -> Text -> JSON.Parser (Maybe Addr)
addrFieldMaybe x f = (Text.Read.readMaybe . Text.unpack) <$> (x .: f)

dataField :: JSON.Object -> Text -> JSON.Parser ByteString
dataField x f = hexText <$> (x .: f)

toWord512 :: W256 -> Word512
toWord512 (W256 x) = fromHiAndLo 0 x

fromWord512 :: Word512 -> W256
fromWord512 x = W256 (loWord x)

{-# SPECIALIZE num :: Word8 -> W256 #-}
num :: (Integral a, Num b) => a -> b
num = fromIntegral

padLeft :: Int -> ByteString -> ByteString
padLeft n xs = BS.replicate (n - BS.length xs) 0 <> xs

padRight :: Int -> ByteString -> ByteString
padRight n xs = xs <> BS.replicate (n - BS.length xs) 0

word :: ByteString -> W256
word xs = case Cereal.runGet m (padLeft 32 xs) of
            Left _ -> error "internal error"
            Right x -> W256 x
  where
    m = do a <- Cereal.getWord64be
           b <- Cereal.getWord64be
           c <- Cereal.getWord64be
           d <- Cereal.getWord64be
           return $ fromHiAndLo (fromHiAndLo a b) (fromHiAndLo c d)

byteAt :: (Bits a, Bits b, Integral a, Num b) => a -> Int -> b
byteAt x j = num (x `shiftR` (j * 8)) .&. 0xff

fromBE :: (Integral a) => ByteString -> a
fromBE xs = if xs == mempty then 0
  else 256 * fromBE (BS.init xs)
       + (num $ BS.last xs)

asBE :: (Integral a) => a -> ByteString
asBE 0 = mempty
asBE x = asBE (x `div` 256)
  <> BS.pack [num $ x `mod` 256]

word256Bytes :: W256 -> ByteString
word256Bytes x = BS.pack [byteAt x (31 - i) | i <- [0..31]]

word160Bytes :: Addr -> ByteString
word160Bytes x = BS.pack [byteAt (addressWord160 x) (19 - i) | i <- [0..19]]

newtype Nibble = Nibble Word8
  deriving ( Num, Integral, Real, Ord, Enum, Eq
    , Bits, FiniteBits, Bounded, Generic)

instance Show Nibble where
  show = (:[]) . intToDigit . num

--Get first and second Nibble from byte
hi, lo :: Word8 -> Nibble
hi b = Nibble $ b `shiftR` 4
lo b = Nibble $ b .&. 0x0f

toByte :: Nibble -> Nibble -> Word8
toByte  (Nibble high) (Nibble low) = high `shift` 4 .|. low

unpackNibbles :: ByteString -> [Nibble]
unpackNibbles bs = BS.unpack bs >>= unpackByte
  where unpackByte b = [hi b, lo b]

--Well-defined for even length lists only (plz dependent types)
packNibbles :: [Nibble] -> ByteString
packNibbles [] = mempty
packNibbles (n1:n2:ns) = BS.singleton (toByte n1 n2) <> packNibbles ns
packNibbles _ = error "cant pack odd number of nibbles"
