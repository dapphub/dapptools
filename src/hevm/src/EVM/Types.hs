{-# Language CPP #-}
{-# Language TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}

module EVM.Types where

import Prelude hiding  (Word)

import Data.Aeson (FromJSON (..), (.:))

#if MIN_VERSION_aeson(1, 0, 0)
import Data.Aeson (FromJSONKey (..), FromJSONKeyFunction (..))
import Data.Aeson
#endif

import Crypto.Hash
import Data.SBV hiding (Word)
import Data.Kind
import Data.Monoid ((<>))
import Data.Bifunctor (first)
import Data.Char
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 as BS16
import Data.ByteString.Builder (byteStringHex, toLazyByteString)
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Char8  as Char8
import Data.DoubleWord
import Data.DoubleWord.TH
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import Numeric (readHex, showHex)
import Options.Generic
import Control.Arrow ((>>>))

import Text.Printf

import qualified Data.ByteArray       as BA
import qualified Data.Aeson           as JSON
import qualified Data.Aeson.Types     as JSON
import qualified Data.ByteString      as BS
import qualified Data.Serialize.Get   as Cereal
import qualified Data.Text            as Text
import qualified Data.Text.Encoding   as Text
import qualified Text.Read

-- Some stuff for "generic programming", needed to create Word512
import Data.Data


data Buffer
  = ConcreteBuffer ByteString
  | SymbolicBuffer [SWord 8]

newtype W256 = W256 Word256
  deriving
    ( Num, Integral, Real, Ord, Enum, Eq
    , Bits, FiniteBits, Bounded, Generic
    )

data Word = C Whiff W256 --maybe to remove completely in the future

instance Show Word where
  show (C _ x) = show x

instance Read Word where
  readsPrec n s =
    case readsPrec n s of
      [(x, r)] -> [(C Dull x, r)]
      _ -> []

w256 :: W256 -> Word
w256 = C Dull

instance Bits Word where
  (C _ x) .&. (C _ y) = w256 (x .&. y)
  (C _ x) .|. (C _ y) = w256 (x .|. y)
  (C _ x) `xor` (C _ y) = w256 (x `xor` y)
  complement (C _ x) = w256 (complement x)
  shift (C _ x) i = w256 (shift x i)
  rotate (C _ x) i = w256 (rotate x i)
  bitSize (C _ x) = bitSize x
  bitSizeMaybe (C _ x) = bitSizeMaybe x
  isSigned (C _ x) = isSigned x
  testBit (C _ x) = testBit x
  bit i = w256 (bit i)
  popCount (C _ x) = popCount x

instance FiniteBits Word where
  finiteBitSize (C _ x) = finiteBitSize x
  countLeadingZeros (C _ x) = countLeadingZeros x
  countTrailingZeros (C _ x) = countTrailingZeros x

instance Bounded Word where
  minBound = w256 minBound
  maxBound = w256 maxBound

instance Eq Word where
  (C _ x) == (C _ y) = x == y

instance Enum Word where
  toEnum i = w256 (toEnum i)
  fromEnum (C _ x) = fromEnum x

instance Integral Word where
  quotRem (C _ x) (C _ y) =
    let (a, b) = quotRem x y
    in (w256 a, w256 b)
  toInteger (C _ x) = toInteger x

instance Num Word where
  (C _ x) + (C _ y) = w256 (x + y)
  (C _ x) * (C _ y) = w256 (x * y)
  abs (C _ x) = w256 (abs x)
  signum (C _ x) = w256 (signum x)
  fromInteger x = w256 (fromInteger x)
  negate (C _ x) = w256 (negate x)

instance Real Word where
  toRational (C _ x) = toRational x

instance Ord Word where
  compare (C _ x) (C _ y) = compare x y


-- | Symbolic words of 256 bits, possibly annotated with additional
--   "insightful" information
data SymWord = S Whiff (SWord 256)

instance Show SymWord where
  show s@(S w _) = case maybeLitWord s of
    Nothing -> case w of
      Dull -> "<symbolic>"
      whiff -> show whiff
    Just w'  -> show w'

-- | Convenience functions transporting between the concrete and symbolic realm
-- TODO - look for all the occurences of sw256 and replace them with manual construction
sw256 :: SWord 256 -> SymWord
sw256 x = S Dull x

-- | Custom instances for SymWord, many of which have direct
-- analogues for concrete words defined in Concrete.hs
instance EqSymbolic SymWord where
  (.==) (S _ x) (S _ y) = x .== y

instance Num SymWord where
  (S a x) + (S b y) = S (InfixBinOp "+" a b) (x + y)
  (S a x) * (S b y) = S (InfixBinOp "*" a b) (x * y)
  abs (S a x) = S (UnOp "abs" a) (abs x)
  signum (S a x) = S (UnOp "signum" a) (signum x)
  fromInteger x = S (Val (show x)) (fromInteger x)
  negate (S a x) = S (UnOp "-" a) (negate x)

instance Bits SymWord where
  (S a x) .&. (S b y) = S (InfixBinOp "&" a b) (x .&. y)
  (S a x) .|. (S b y) = S (InfixBinOp "|" a b) (x .|. y)
  (S a x) `xor` (S b y) = S (InfixBinOp "xor" a b) (x `xor` y)
  complement (S a x) = S (UnOp "~" a) (complement x)
  shift (S a x) i = S (UnOp ("<<" ++ (show i) ++ " ") a ) (shift x i)
  rotate (S a x) i = S (UnOp ("rotate " ++ (show i) ++ " ") a) (rotate x i)
  bitSize (S _ x) = bitSize x
  bitSizeMaybe (S _ x) = bitSizeMaybe x
  isSigned (S _ x) = isSigned x
  testBit (S _ x) i = testBit x i
  bit i = sw256 (bit i)
  popCount (S _ x) = popCount x

instance SDivisible SymWord where
  sQuotRem (S _ x) (S _ y) = let (a, b) = x `sQuotRem` y
                             in (sw256 a, sw256 b)
  sDivMod (S _ x) (S _ y) = let (a, b) = x `sDivMod` y
                             in (sw256 a, sw256 b)

instance Mergeable SymWord where
  symbolicMerge a b (S wx x) (S _ y) = S wx (symbolicMerge a b x y)
  select xs (S _ x) b = let ys = fmap (\(S _ y) -> y) xs
                        in sw256 $ select ys x b

instance Bounded SymWord where
  minBound = sw256 minBound
  maxBound = sw256 maxBound

instance Eq SymWord where
  (S _ x) == (S _ y) = x == y

instance Enum SymWord where
  toEnum i = sw256 (toEnum i)
  fromEnum (S _ x) = fromEnum x

instance OrdSymbolic SymWord where
  (.<) (S _ x) (S _ y) = (.<) x y


-- | This type can give insight into the provenance of a term
data Whiff = Dull
           | Val String
           | FromKeccak ByteString
           | Var String
           | FromBytes SymWord Buffer
           | FromCalldata SymWord Buffer
           | FromStorage Whiff
           | InfixBinOp String Whiff Whiff
           | BinOp String Whiff Whiff
           | UnOp String Whiff

instance Show Whiff where
  show Dull = "<symbolic>"
  show (Val s) = s
  show (FromKeccak bstr) = "FromKeccak " ++ show bstr
  show (Var x) = printf "<%s>" x
  show (FromBytes index buf) = "FromBuffer " ++ (show index) ++ " " ++ show buf
  show (FromCalldata index buf) = "FromCalldata " ++ (show index) ++ " " ++ show buf
  show (FromStorage w) = "SREAD(" ++ show w ++ ")"
  show (InfixBinOp op a b) = printf "(%s %s %s)" (show a) op (show b)
  show (BinOp op a b) = printf "%s(%s, %s)" op (show a) (show b)
  show (UnOp op x) = op ++ "(" ++ (show x) ++ ")"

newtype Addr = Addr { addressWord160 :: Word160 }
  deriving (Num, Integral, Real, Ord, Enum, Eq, Bits, Generic)

newtype SAddr = SAddr { saddressWord160 :: SWord 160 }
  deriving (Num)

-- | Capture the correspondence between sized and fixed-sized BVs
type family FromSizzle (t :: Type) :: Type where
   FromSizzle (WordN 256) = W256
   FromSizzle (WordN 160) = Addr

-- | Conversion from a sized BV to a fixed-sized bit-vector.
class FromSizzleBV a where
   -- | Convert a sized bit-vector to the corresponding fixed-sized bit-vector,
   -- for instance 'SWord 16' to 'SWord16'. See also 'toSized'.
   fromSizzle :: a -> FromSizzle a

   default fromSizzle :: (Num (FromSizzle a), Integral a) => a -> FromSizzle a
   fromSizzle = fromIntegral

maybeLitWord :: SymWord -> Maybe Word
maybeLitWord (S whiff a) = fmap (C whiff . fromSizzle) (unliteral a)

-- We need a 512-bit word for doing ADDMOD and MULMOD with full precision.
mkUnpackedDoubleWord "Word512" ''Word256 "Int512" ''Int256 ''Word256
  [''Typeable, ''Data, ''Generic]


-- | convert between (WordN 256) and Word256
type family ToSizzle (t :: Type) :: Type where
    ToSizzle W256 = (WordN 256)
    ToSizzle Addr = (WordN 160)

-- | Conversion from a fixed-sized BV to a sized bit-vector.
class ToSizzleBV a where
   -- | Convert a fixed-sized bit-vector to the corresponding sized bit-vector,
   toSizzle :: a -> ToSizzle a

   default toSizzle :: (Num (ToSizzle a), Integral a) => (a -> ToSizzle a)
   toSizzle = fromIntegral


instance (ToSizzleBV W256)
instance (FromSizzleBV (WordN 256))
instance (ToSizzleBV Addr)
instance (FromSizzleBV (WordN 160))


litBytes :: ByteString -> [SWord 8]
litBytes bs = fmap (toSized . literal) (BS.unpack bs)

-- | Operations over buffers (concrete or symbolic)

-- | A buffer is a list of bytes. For concrete execution, this is simply `ByteString`.
-- In symbolic settings, it is a list of symbolic bitvectors of size 8.
instance Show Buffer where
  show (ConcreteBuffer b) = show $ ByteStringS b
  show (SymbolicBuffer b) = show (length b) ++ " bytes"


instance Semigroup Buffer where
  ConcreteBuffer a <> ConcreteBuffer b = ConcreteBuffer (a <> b)
  ConcreteBuffer a <> SymbolicBuffer b = SymbolicBuffer (litBytes a <> b)
  SymbolicBuffer a <> ConcreteBuffer b = SymbolicBuffer (a <> litBytes b)
  SymbolicBuffer a <> SymbolicBuffer b = SymbolicBuffer (a <> b)

instance Monoid Buffer where
  mempty = ConcreteBuffer mempty

instance EqSymbolic Buffer where
  ConcreteBuffer a .== ConcreteBuffer b = literal (a == b)
  ConcreteBuffer a .== SymbolicBuffer b = litBytes a .== b
  SymbolicBuffer a .== ConcreteBuffer b = a .== litBytes b
  SymbolicBuffer a .== SymbolicBuffer b = a .== b


instance Read W256 where
  readsPrec _ "0x" = [(0, "")]
  readsPrec n s = first W256 <$> readsPrec n s

instance Show W256 where
  showsPrec _ s = ("0x" ++) . showHex s

instance JSON.ToJSON W256 where
  toJSON = JSON.String . Text.pack . show

instance JSON.ToJSON Word where
  toJSON (C _ x) = toJSON x

instance Read Addr where
  readsPrec _ ('0':'x':s) = readHex s
  readsPrec _ s = readHex s

instance Show Addr where
  showsPrec _ addr next =
    let hex = showHex addr next
        str = replicate (40 - length hex) '0' ++ hex
    in "0x" ++ toChecksumAddress str

instance Show SAddr where
  show (SAddr a) = case unliteral a of
    Nothing -> "<symbolic addr>"
    Just c -> show $ fromSizzle c

-- https://eips.ethereum.org/EIPS/eip-55
toChecksumAddress :: String -> String
toChecksumAddress addr = zipWith transform nibbles addr
  where
    nibbles = unpackNibbles . BS.take 20 $ keccakBytes (Char8.pack addr)
    transform nibble = if nibble >= 8 then toUpper else id

strip0x :: ByteString -> ByteString
strip0x bs = if "0x" `Char8.isPrefixOf` bs then Char8.drop 2 bs else bs

newtype ByteStringS = ByteStringS ByteString deriving (Eq)

instance Show ByteStringS where
  show (ByteStringS x) = ("0x" ++) . Text.unpack . fromBinary $ x
    where
      fromBinary =
        Text.decodeUtf8 . toStrict . toLazyByteString . byteStringHex

instance Read ByteStringS where
    readsPrec _ ('0':'x':x) = [bimap ByteStringS (Text.unpack . Text.decodeUtf8) bytes]
       where bytes = BS16.decode (Text.encodeUtf8 (Text.pack x))
    readsPrec _ _ = []

instance JSON.ToJSON ByteStringS where
  toJSON = JSON.String . Text.pack . show

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
readNull x = fromMaybe x . Text.Read.readMaybe

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

truncpad :: Int -> [SWord 8] -> [SWord 8]
truncpad n xs = if m > n then take n xs
                else mappend xs (replicate (n - m) 0)
  where m = length xs

word256 :: ByteString -> Word256
word256 xs = case Cereal.runGet m (padLeft 32 xs) of
               Left _ -> error "internal error"
               Right x -> x
  where
    m = do a <- Cereal.getWord64be
           b <- Cereal.getWord64be
           c <- Cereal.getWord64be
           d <- Cereal.getWord64be
           return $ fromHiAndLo (fromHiAndLo a b) (fromHiAndLo c d)

word :: ByteString -> W256
word = W256 . word256

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
packNibbles _ = error "can't pack odd number of nibbles"

-- Keccak hashing

keccakBytes :: ByteString -> ByteString
keccakBytes =
  (hash :: ByteString -> Digest Keccak_256)
    >>> BA.unpack
    >>> BS.pack

word32 :: [Word8] -> Word32
word32 xs = sum [ fromIntegral x `shiftL` (8*n)
                | (n, x) <- zip [0..] (reverse xs) ]

keccak :: ByteString -> W256
keccak =
  keccakBytes
    >>> BS.take 32
    >>> word

abiKeccak :: ByteString -> Word32
abiKeccak =
  keccakBytes
    >>> BS.take 4
    >>> BS.unpack
    >>> word32
