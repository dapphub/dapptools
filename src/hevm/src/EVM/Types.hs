{-# Language CPP #-}
{-# Language TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}

module EVM.Types where

import Prelude hiding  (Word, LT, GT)

import Data.Aeson
import Crypto.Hash
import Data.SBV hiding (Word)
import Data.Kind
import Data.Bifunctor (first)
import Data.Char
import Data.List (intercalate)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 as BS16
import Data.ByteString.Builder (byteStringHex, toLazyByteString)
import Data.ByteString.Lazy (toStrict)
import Control.Monad.State.Strict (liftM)
import qualified Data.ByteString.Char8  as Char8
import Data.DoubleWord
import Data.DoubleWord.TH
import Data.Maybe (fromMaybe)
import Numeric (readHex, showHex)
import Options.Generic
import Control.Arrow ((>>>))

import qualified Data.ByteArray       as BA
import qualified Data.Aeson           as JSON
import qualified Data.Aeson.Types     as JSON
import qualified Data.ByteString      as BS
import qualified Data.Serialize.Get   as Cereal
import qualified Data.Text            as Text
import qualified Data.Text.Encoding   as Text
import qualified Data.Sequence        as Seq
import qualified Text.Regex.TDFA      as Regex
import qualified Text.Read

-- Some stuff for "generic programming", needed to create Word512
import Data.Data

-- We need a 512-bit word for doing ADDMOD and MULMOD with full precision.
mkUnpackedDoubleWord "Word512" ''Word256 "Int512" ''Int256 ''Word256
  [''Typeable, ''Data, ''Generic]


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
      [(x, r)] -> [(C (Literal x) x, r)]
      _ -> []

w256 :: W256 -> Word
w256 w = C (Literal w) w

instance Bits Word where
  (C a x) .&. (C b y) = C (And a b) (x .&. y)
  (C a x) .|. (C b y) = C (Or  a b) (x .|. y)
  (C a x) `xor` (C b y) = C (Todo "xor" [a, b]) (x `xor` y)
  complement (C a x) = C (Neg a) (complement x)
  shiftL (C a x) i = C (SHL a (Literal $ fromIntegral i)) (shiftL x i)
  shiftR (C a x) i = C (SHR a (Literal $ fromIntegral i)) (shiftR x i)
  rotate (C a x) i = C (Todo "rotate " [a]) (rotate x i) -- unused.
  bitSize (C _ x) = bitSize x
  bitSizeMaybe (C _ x) = bitSizeMaybe x
  isSigned (C _ x) = isSigned x
  testBit (C _ x) i = testBit x i
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
  (C a x) + (C b y) = C (Add a b) (x + y)
  (C a x) * (C b y) = C (Mul a b) (x * y)
  abs (C a x) = C (Todo "abs" [a]) (abs x)
  signum (C a x) = C (Todo "signum" [a]) (signum x)
  fromInteger x = C (Literal (fromInteger x)) (fromInteger x)
  negate (C a x) = C (Sub (Literal 0) a) (negate x)

instance Real Word where
  toRational (C _ x) = toRational x

instance Ord Word where
  compare (C _ x) (C _ y) = compare x y

newtype ByteStringS = ByteStringS ByteString deriving (Eq)

instance Show ByteStringS where
  show (ByteStringS x) = ("0x" ++) . Text.unpack . fromBinary $ x
    where
      fromBinary =
        Text.decodeUtf8 . toStrict . toLazyByteString . byteStringHex

instance JSON.ToJSON ByteStringS where
  toJSON = JSON.String . Text.pack . show

-- | Symbolic words of 256 bits, possibly annotated with additional
--   "insightful" information
data SymWord = S Whiff (SWord 256)

instance Show SymWord where
  show (S w _) = show w

var :: String -> SWord 256 -> SymWord
var name x = S (Var name x) x

-- | Custom instances for SymWord, many of which have direct
-- analogues for concrete words defined in Concrete.hs
instance EqSymbolic SymWord where
  (.==) (S _ x) (S _ y) = x .== y

instance Num SymWord where
  (S a x) + (S b y) = S (Add a b) (x + y)
  (S a x) * (S b y) = S (Mul a b) (x * y)
  abs (S a x) = S (Todo "abs" [a]) (abs x)
  signum (S a x) = S (Todo "signum" [a]) (signum x)
  fromInteger x = S (Literal (fromInteger x)) (fromInteger x)
  negate (S a x) = S (Neg a) (negate x)

instance Bits SymWord where
  (S a x) .&. (S b y) = S (And a b) (x .&. y)
  (S a x) .|. (S b y) = S (Or  a b) (x .|. y)
  (S a x) `xor` (S b y) = S (Todo "xor" [a, b]) (x `xor` y)
  complement (S a x) = S (Neg a) (complement x)
  shiftL (S a x) i = S (SHL a (Literal $ fromIntegral i)) (shiftL x i)
  shiftR (S a x) i = S (SHR a (Literal $ fromIntegral i)) (shiftR x i)
  rotate (S a x) i = S (Todo "rotate " [a]) (rotate x i) -- unused.
  bitSize (S _ x) = bitSize x
  bitSizeMaybe (S _ x) = bitSizeMaybe x
  isSigned (S _ x) = isSigned x
  testBit (S _ x) i = testBit x i
  bit i = w256lit (bit i)
  popCount (S _ x) = popCount x

-- sQuotRem and sDivMod are identical for SWord 256
-- prove $ \x y -> x `sQuotRem` (y :: SWord 256) .== x `sDivMod` y
-- Q.E.D.
instance SDivisible SymWord where
  sQuotRem (S x' x) (S y' y) = let (a, b) = x `sQuotRem` y
                               in (S (Div x' y') a, S (Mod x' y') b)
  sDivMod = sQuotRem

-- | Instead of supporting a Mergeable instance directly,
-- we use one which carries the Whiff around:
iteWhiff :: Whiff -> SBool -> SWord 256 -> SWord 256 -> SymWord
iteWhiff w b x y = S w (ite b x y)

instance Bounded SymWord where
  minBound = w256lit minBound
  maxBound = w256lit maxBound

instance Eq SymWord where
  (S _ x) == (S _ y) = x == y

instance Enum SymWord where
  toEnum i = w256lit (toEnum i)
  fromEnum (S _ x) = fromEnum x

-- | This type can give insight into the provenance of a term
-- which is useful, both for the aesthetic purpose of printing
-- terms in a richer way, but also do optimizations on the AST
-- instead of letting the SMT solver do all the heavy lifting.
data Whiff =
  Todo String [Whiff]
  -- booleans / bits
  | And  Whiff Whiff
  | Or   Whiff Whiff
  | Eq   Whiff Whiff
  | LT   Whiff Whiff
  | GT   Whiff Whiff
  | SLT  Whiff Whiff
  | SGT  Whiff Whiff
  | IsZero Whiff
  | ITE Whiff Whiff Whiff
  -- bits
  | SHL Whiff Whiff
  | SHR Whiff Whiff
  | SAR Whiff Whiff

  -- integers
  | Add  Whiff Whiff
  | Sub  Whiff Whiff
  | Mul  Whiff Whiff
  | Div  Whiff Whiff
  | Mod  Whiff Whiff
  | Exp  Whiff Whiff
  | Neg  Whiff
  | FromKeccak Buffer
  | FromBytes Buffer
  | FromStorage Whiff (SArray (WordN 256) (WordN 256))
  | Literal W256
  | Var String (SWord 256)

instance Show Whiff where
  show w =
    let
      infix' s x y = show x ++ s ++ show y
    in case w of
      Todo s args -> s ++ "(" ++ (intercalate "," (show <$> args)) ++ ")"
      And x y     -> infix' " and " x y
      Or x y      -> infix' " or " x y
      ITE b x y  -> "if " ++ show b ++ " then " ++ show x ++ " else " ++ show y
      Eq x y      -> infix' " == " x y
      LT x y      -> infix' " < " x y
      GT x y      -> infix' " > " x y
      SLT x y     -> infix' " s< " x y
      SGT x y     -> infix' " s> " x y
      IsZero x    -> "IsZero(" ++ show x ++ ")"
      SHL x y     -> infix' " << " x y
      SHR x y     -> infix' " >> " x y
      SAR x y     -> infix' " a>> " x y
      Add x y     -> infix' " + " x y
      Sub x y     -> infix' " - " x y
      Mul x y     -> infix' " * " x y
      Div x y     -> infix' " / " x y
      Mod x y     -> infix' " % " x y
      Exp x y     -> infix' " ** " x y
      Neg x       -> "not " ++ show x
      Var v _     -> v
      FromKeccak buf -> "keccak(" ++ show buf ++ ")"
      Literal x -> show x
      FromBytes buf -> "FromBuffer " ++ show buf
      FromStorage l _ -> "SLOAD(" ++ show l ++ ")"

newtype Addr = Addr { addressWord160 :: Word160 }
  deriving (Num, Integral, Real, Ord, Enum, Eq, Bits, Generic)

newtype SAddr = SAddr { saddressWord160 :: SWord 160 }
  deriving (Num)

-- | Capture the correspondence between sized and fixed-sized BVs
-- (This is blatant copypasta of `FromSized` from sbv, which just
-- happens to be defined up to 64 bits)
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

w256lit :: W256 -> SymWord
w256lit x = S (Literal x) $ literal $ toSizzle x

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
    in "0x" ++ toChecksumAddress str ++ drop 40 str

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
    Right x -> x
    _ -> error ("invalid hex bytestring for " ++ msg)

hexText :: Text -> ByteString
hexText t =
  case BS16.decode (Text.encodeUtf8 (Text.drop 2 t)) of
    Right x -> x
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

padRight' :: Int -> String -> String
padRight' n xs = xs <> replicate (n - length xs) '0'

-- | Right padding  / truncating
truncpad :: Int -> [SWord 8] -> [SWord 8]
truncpad n xs = if m > n then take n xs
                else mappend xs (replicate (n - m) 0)
  where m = length xs

padLeft' :: (Num a) => Int -> [a] -> [a]
padLeft' n xs = replicate (n - length xs) 0 <> xs

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

-- Utils

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = liftM concat (mapM f xs)

regexMatches :: Text -> Text -> Bool
regexMatches regexSource =
  let
    compOpts =
      Regex.defaultCompOpt { Regex.lastStarGreedy = True }
    execOpts =
      Regex.defaultExecOpt { Regex.captureGroups = False }
    regex = Regex.makeRegexOpts compOpts execOpts (Text.unpack regexSource)
  in
    Regex.matchTest regex . Seq.fromList . Text.unpack
