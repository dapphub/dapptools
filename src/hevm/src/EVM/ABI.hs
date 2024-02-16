{-

  The ABI encoding is mostly straightforward.

  Definition: an int-like value is an uint, int, boolean, or address.

  Basic encoding:

    * Int-likes and length prefixes are big-endian.
    * All values are right-0-padded to multiples of 256 bits.
      - Bytestrings are padded as a whole; e.g., bytes[33] takes 64 bytes.
    * Dynamic-length sequences are prefixed with their length.

  Sequences are encoded as a head followed by a tail, thus:

    * the tail is the concatenation of encodings of non-int-like items.
    * the head has 256 bits per sequence item, thus:
      - int-likes are stored directly;
      - non-int-likes are stored as byte offsets into the tail,
          starting from the beginning of the head.

  Nested sequences are encoded recursively with no special treatment.

  Calldata args are encoded as heterogeneous sequences sans length prefix.

-}

{-# Language StrictData #-}
{-# Language DataKinds #-}

module EVM.ABI
  ( AbiValue (..)
  , AbiType (..)
  , AbiKind (..)
  , AbiVals (..)
  , abiKind
  , Event (..)
  , SolError (..)
  , Anonymity (..)
  , Indexed (..)
  , putAbi
  , getAbi
  , getAbiSeq
  , genAbiValue
  , abiValueType
  , abiTypeSolidity
  , abiMethod
  , emptyAbi
  , encodeAbiValue
  , decodeAbiValue
  , decodeStaticArgs
  , decodeBuffer
  , formatString
  , parseTypeName
  , makeAbiValue
  , parseAbiValue
  , selector
  ) where

import EVM.Types

import Control.Monad      (replicateM, replicateM_, forM_, void)
import Data.Binary.Get    (Get, runGet, runGetOrFail, label, getWord8, getWord32be, skip)
import Data.Binary.Put    (Put, runPut, putWord8, putWord32be)
import Data.Bits          (shiftL, shiftR, (.&.))
import Data.ByteString    (ByteString)
import Data.Char          (isHexDigit)
import Data.DoubleWord    (Word256, Int256, signedWord)
import Data.Functor       (($>))
import Data.Text          (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8, decodeUtf8')
import Data.Vector        (Vector, toList)
import Data.Word          (Word32)
import Data.List          (intercalate)
import Data.SBV           (fromBytes)
import GHC.Generics

import Test.QuickCheck hiding ((.&.), label)
import Text.ParserCombinators.ReadP
import Control.Applicative

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as BS16
import qualified Data.ByteString.Char8  as Char8
import qualified Data.ByteString.Lazy   as BSLazy
import qualified Data.Text              as Text
import qualified Data.Vector            as Vector

import qualified Text.Megaparsec      as P
import qualified Text.Megaparsec.Char as P

data AbiValue
  = AbiUInt         Int Word256
  | AbiInt          Int Int256
  | AbiAddress      Addr
  | AbiBool         Bool
  | AbiBytes        Int BS.ByteString
  | AbiBytesDynamic BS.ByteString
  | AbiString       BS.ByteString
  | AbiArrayDynamic AbiType (Vector AbiValue)
  | AbiArray        Int AbiType (Vector AbiValue)
  | AbiTuple        (Vector AbiValue)
  deriving (Read, Eq, Ord, Generic)

-- | Pretty-print some 'AbiValue'.
instance Show AbiValue where
  show (AbiUInt _ n)         = show n
  show (AbiInt  _ n)         = show n
  show (AbiAddress n)        = show n
  show (AbiBool b)           = if b then "true" else "false"
  show (AbiBytes      _ b)   = show (ByteStringS b)
  show (AbiBytesDynamic b)   = show (ByteStringS b)
  show (AbiString       s)   = formatString s
  show (AbiArrayDynamic _ v) =
    "[" ++ intercalate ", " (show <$> Vector.toList v) ++ "]"
  show (AbiArray      _ _ v) =
    "[" ++ intercalate ", " (show <$> Vector.toList v) ++ "]"
  show (AbiTuple v) =
    "(" ++ intercalate ", " (show <$> Vector.toList v) ++ ")"

formatString :: ByteString -> String
formatString bs =
  case decodeUtf8' (fst (BS.spanEnd (== 0) bs)) of
    Right s -> "\"" <> unpack s <> "\""
    Left _ -> "❮utf8 decode failed❯: " <> (show $ ByteStringS bs)

data AbiType
  = AbiUIntType         Int
  | AbiIntType          Int
  | AbiAddressType
  | AbiBoolType
  | AbiBytesType        Int
  | AbiBytesDynamicType
  | AbiStringType
  | AbiArrayDynamicType AbiType
  | AbiArrayType        Int AbiType
  | AbiTupleType        (Vector AbiType)
  deriving (Read, Eq, Ord, Generic)

instance Show AbiType where
  show = Text.unpack . abiTypeSolidity

data AbiKind = Dynamic | Static
  deriving (Show, Read, Eq, Ord, Generic)

data Anonymity = Anonymous | NotAnonymous
  deriving (Show, Ord, Eq, Generic)
data Indexed   = Indexed   | NotIndexed
  deriving (Show, Ord, Eq, Generic)
data Event     = Event Text Anonymity [(Text, AbiType, Indexed)]
  deriving (Show, Ord, Eq, Generic)
data SolError  = SolError Text [AbiType]
  deriving (Show, Ord, Eq, Generic)

abiKind :: AbiType -> AbiKind
abiKind = \case
  AbiBytesDynamicType   -> Dynamic
  AbiStringType         -> Dynamic
  AbiArrayDynamicType _ -> Dynamic
  AbiArrayType _ t      -> abiKind t
  AbiTupleType ts       -> if Dynamic `elem` (abiKind <$> ts) then Dynamic else Static
  _                     -> Static

abiValueType :: AbiValue -> AbiType
abiValueType = \case
  AbiUInt n _         -> AbiUIntType n
  AbiInt n _          -> AbiIntType  n
  AbiAddress _        -> AbiAddressType
  AbiBool _           -> AbiBoolType
  AbiBytes n _        -> AbiBytesType n
  AbiBytesDynamic _   -> AbiBytesDynamicType
  AbiString _         -> AbiStringType
  AbiArrayDynamic t _ -> AbiArrayDynamicType t
  AbiArray n t _      -> AbiArrayType n t
  AbiTuple v          -> AbiTupleType (abiValueType <$> v)

abiTypeSolidity :: AbiType -> Text
abiTypeSolidity = \case
  AbiUIntType n         -> "uint" <> pack (show n)
  AbiIntType n          -> "int" <> pack (show n)
  AbiAddressType        -> "address"
  AbiBoolType           -> "bool"
  AbiBytesType n        -> "bytes" <> pack (show n)
  AbiBytesDynamicType   -> "bytes"
  AbiStringType         -> "string"
  AbiArrayDynamicType t -> abiTypeSolidity t <> "[]"
  AbiArrayType n t      -> abiTypeSolidity t <> "[" <> pack (show n) <> "]"
  AbiTupleType ts       -> "(" <> (Text.intercalate "," . Vector.toList $ abiTypeSolidity <$> ts) <> ")"

getAbi :: AbiType -> Get AbiValue
getAbi t = label (Text.unpack (abiTypeSolidity t)) $
  case t of
    AbiUIntType n  -> do
      let word32Count = 8 * div (n + 255) 256
      xs <- replicateM word32Count getWord32be
      pure (AbiUInt n (pack32 word32Count xs))

    AbiIntType n   -> asUInt n (AbiInt n)
    AbiAddressType -> asUInt 256 AbiAddress
    AbiBoolType    -> asUInt 256 (AbiBool . (> (0 :: Integer)))

    AbiBytesType n ->
      AbiBytes n <$> getBytesWith256BitPadding n

    AbiBytesDynamicType ->
      AbiBytesDynamic <$>
        (label "bytes length prefix" getWord256
          >>= label "bytes data" . getBytesWith256BitPadding)

    AbiStringType -> do
      AbiString <$>
        (label "string length prefix" getWord256
          >>= label "string data" . getBytesWith256BitPadding)

    AbiArrayType n t' ->
      AbiArray n t' <$> getAbiSeq n (repeat t')

    AbiArrayDynamicType t' -> do
      AbiUInt _ n <- label "array length" (getAbi (AbiUIntType 256))
      AbiArrayDynamic t' <$>
        label "array body" (getAbiSeq (fromIntegral n) (repeat t'))

    AbiTupleType ts ->
      AbiTuple <$> getAbiSeq (Vector.length ts) (Vector.toList ts)

putAbi :: AbiValue -> Put
putAbi = \case
  AbiUInt _ x ->
    forM_ (reverse [0 .. 7]) $ \i ->
      putWord32be (fromIntegral (shiftR x (i * 32) .&. 0xffffffff))

  AbiInt n x   -> putAbi (AbiUInt n (fromIntegral x))
  AbiAddress x -> putAbi (AbiUInt 160 (fromIntegral x))
  AbiBool x    -> putAbi (AbiUInt 8 (if x then 1 else 0))

  AbiBytes n xs -> do
    forM_ [0 .. n-1] (putWord8 . BS.index xs)
    replicateM_ (roundTo32Bytes n - n) (putWord8 0)

  AbiBytesDynamic xs -> do
    let n = BS.length xs
    putAbi (AbiUInt 256 (fromIntegral n))
    putAbi (AbiBytes n xs)

  AbiString s ->
    putAbi (AbiBytesDynamic s)

  AbiArray _ _ xs ->
    putAbiSeq xs

  AbiArrayDynamic _ xs -> do
    putAbi (AbiUInt 256 (fromIntegral (Vector.length xs)))
    putAbiSeq xs

  AbiTuple v ->
    putAbiSeq v

-- | Decode a sequence type (e.g. tuple / array). Will fail for non sequence types
getAbiSeq :: Int -> [AbiType] -> Get (Vector AbiValue)
getAbiSeq n ts = label "sequence" $ do
  hs <- label "sequence head" (getAbiHead n ts)
  Vector.fromList <$>
    label "sequence tail" (mapM (either getAbi pure) hs)

getAbiHead :: Int -> [AbiType]
  -> Get [Either AbiType AbiValue]
getAbiHead 0 _      = pure []
getAbiHead _ []     = fail "ran out of types"
getAbiHead n (t:ts) =
  case abiKind t of
    Dynamic ->
      (Left t :) <$> (skip 32 *> getAbiHead (n - 1) ts)
    Static ->
      do x  <- getAbi t
         xs <- getAbiHead (n - 1) ts
         pure (Right x : xs)

putAbiTail :: AbiValue -> Put
putAbiTail x =
  case abiKind (abiValueType x) of
    Static  -> pure ()
    Dynamic -> putAbi x

abiTailSize :: AbiValue -> Int
abiTailSize x =
  case abiKind (abiValueType x) of
    Static -> 0
    Dynamic ->
      case x of
        AbiString s -> 32 + roundTo32Bytes (BS.length s)
        AbiBytesDynamic s -> 32 + roundTo32Bytes (BS.length s)
        AbiArrayDynamic _ xs -> 32 + sum ((abiHeadSize <$> xs) <> (abiTailSize <$> xs))
        AbiArray _ _ xs -> sum ((abiHeadSize <$> xs) <> (abiTailSize <$> xs))
        AbiTuple v -> sum ((abiHeadSize <$> v) <> (abiTailSize <$> v))
        _ -> error "impossible"

abiHeadSize :: AbiValue -> Int
abiHeadSize x =
  case abiKind (abiValueType x) of
    Dynamic -> 32
    Static ->
      case x of
        AbiUInt _ _  -> 32
        AbiInt  _ _  -> 32
        AbiBytes n _ -> roundTo32Bytes n
        AbiAddress _ -> 32
        AbiBool _    -> 32
        AbiTuple v   -> sum (abiHeadSize <$> v)
        AbiArray _ _ xs -> sum (abiHeadSize <$> xs)
        _ -> error "impossible"

putAbiSeq :: Vector AbiValue -> Put
putAbiSeq xs =
  do putHeads headSize $ toList xs
     Vector.sequence_ (putAbiTail <$> xs)
  where
    headSize = Vector.sum $ Vector.map abiHeadSize xs
    putHeads _ [] = pure ()
    putHeads offset (x:xs') =
      case abiKind (abiValueType x) of
        Static -> do putAbi x
                     putHeads offset xs'
        Dynamic -> do putAbi (AbiUInt 256 (fromIntegral offset))
                      putHeads (offset + abiTailSize x) xs'

encodeAbiValue :: AbiValue -> BS.ByteString
encodeAbiValue = BSLazy.toStrict . runPut . putAbi

decodeAbiValue :: AbiType -> BSLazy.ByteString -> AbiValue
decodeAbiValue = runGet . getAbi

selector :: Text -> BS.ByteString
selector s = BSLazy.toStrict . runPut $ putWord32be (abiKeccak (encodeUtf8 s))

abiMethod :: Text -> AbiValue -> BS.ByteString
abiMethod s args = BSLazy.toStrict . runPut $ do
  putWord32be (abiKeccak (encodeUtf8 s))
  putAbi args

parseTypeName :: Vector AbiType -> Text -> Maybe AbiType
parseTypeName = P.parseMaybe . typeWithArraySuffix

typeWithArraySuffix :: Vector AbiType -> P.Parsec () Text AbiType
typeWithArraySuffix v = do
  base <- basicType v
  sizes <-
    P.many $
      P.between
        (P.char '[') (P.char ']')
        (P.many P.digitChar)

  let
    parseSize :: AbiType -> String -> AbiType
    parseSize t "" = AbiArrayDynamicType t
    parseSize t s  = AbiArrayType (read s) t

  pure (foldl parseSize base sizes)

basicType :: Vector AbiType -> P.Parsec () Text AbiType
basicType v =
  P.choice
    [ P.string "address" $> AbiAddressType
    , P.string "bool"    $> AbiBoolType
    , P.string "string"  $> AbiStringType

    , sizedType "uint" AbiUIntType
    , sizedType "int"  AbiIntType
    , sizedType "bytes" AbiBytesType

    , P.string "bytes" $> AbiBytesDynamicType
    , P.string "tuple" $> AbiTupleType v
    ]

  where
    sizedType :: Text -> (Int -> AbiType) -> P.Parsec () Text AbiType
    sizedType s f = P.try $ do
      void (P.string s)
      fmap (f . read) (P.some P.digitChar)

pack32 :: Int -> [Word32] -> Word256
pack32 n xs =
  sum [ shiftL x ((n - i) * 32)
      | (x, i) <- zip (map fromIntegral xs) [1..] ]

asUInt :: Integral i => Int -> (i -> a) -> Get a
asUInt n f = (\(AbiUInt _ x) -> f (fromIntegral x)) <$> getAbi (AbiUIntType n)

getWord256 :: Get Word256
getWord256 = pack32 8 <$> replicateM 8 getWord32be

roundTo32Bytes :: Integral a => a -> a
roundTo32Bytes n = 32 * div (n + 31) 32

emptyAbi :: AbiValue
emptyAbi = AbiTuple mempty

getBytesWith256BitPadding :: Integral a => a -> Get ByteString
getBytesWith256BitPadding i =
  (BS.pack <$> replicateM n getWord8)
    <* skip ((roundTo32Bytes n) - n)
  where n = fromIntegral i

-- QuickCheck instances

genAbiValue :: AbiType -> Gen AbiValue
genAbiValue = \case
   AbiUIntType n -> genUInt n
   AbiIntType n ->
     do a <- genUInt n
        let AbiUInt _ x = a
        pure $ AbiInt n (signedWord (x - 2^(n-1)))
   AbiAddressType ->
     (\(AbiUInt _ x) -> AbiAddress (fromIntegral x)) <$> genUInt 20
   AbiBoolType ->
     elements [AbiBool False, AbiBool True]
   AbiBytesType n ->
     do xs <- replicateM n arbitrary
        pure (AbiBytes n (BS.pack xs))
   AbiBytesDynamicType ->
     AbiBytesDynamic . BS.pack <$> listOf arbitrary
   AbiStringType ->
     AbiString . BS.pack <$> listOf arbitrary
   AbiArrayDynamicType t ->
     do xs <- listOf1 (scale (`div` 2) (genAbiValue t))
        pure (AbiArrayDynamic t (Vector.fromList xs))
   AbiArrayType n t ->
     AbiArray n t . Vector.fromList <$>
       replicateM n (scale (`div` 2) (genAbiValue t))
   AbiTupleType ts ->
     AbiTuple <$> mapM genAbiValue ts
  where
    genUInt n = AbiUInt n <$> arbitraryIntegralWithMax (2^n-1)

instance Arbitrary AbiType where
  arbitrary = oneof $ -- doesn't create any tuples
    [ (AbiUIntType . (* 8)) <$> choose (1, 32)
    , (AbiIntType . (* 8)) <$> choose (1, 32)
    , pure AbiAddressType
    , pure AbiBoolType
    , AbiBytesType <$> choose (1,32)
    , pure AbiBytesDynamicType
    , pure AbiStringType
    , AbiArrayDynamicType <$> scale (`div` 2) arbitrary
    , AbiArrayType
        <$> (getPositive <$> arbitrary)
        <*> scale (`div` 2) arbitrary
    ]

instance Arbitrary AbiValue where
  arbitrary = arbitrary >>= genAbiValue
  shrink = \case
    AbiArrayDynamic t v ->
      Vector.toList v ++
        map (AbiArrayDynamic t . Vector.fromList)
            (shrinkList shrink (Vector.toList v))
    AbiBytesDynamic b -> AbiBytesDynamic . BS.pack <$> shrinkList shrinkIntegral (BS.unpack b)
    AbiString b -> AbiString . BS.pack <$> shrinkList shrinkIntegral (BS.unpack b)
    AbiBytes n a | n <= 32 -> shrink $ AbiUInt (n * 8) (word256 a)
    --bytesN for N > 32 don't really exist right now anyway..
    AbiBytes _ _ | otherwise -> []
    AbiArray _ t v ->
      Vector.toList v ++
        map (\x -> AbiArray (length x) t (Vector.fromList x))
            (shrinkList shrink (Vector.toList v))
    AbiTuple v -> Vector.toList $ AbiTuple . Vector.fromList . shrink <$> v
    AbiUInt n a -> AbiUInt n <$> (shrinkIntegral a)
    AbiInt n a -> AbiInt n <$> (shrinkIntegral a)
    AbiBool b -> AbiBool <$> shrink b
    AbiAddress a -> [AbiAddress 0xacab, AbiAddress 0xdeadbeef, AbiAddress 0xbabeface]
      <> (AbiAddress <$> shrinkIntegral a)


-- Bool synonym with custom read instance
-- to be able to parse lower case 'false' and 'true'
data Boolz = Boolz Bool

instance Read Boolz where
  readsPrec _ ('T':'r':'u':'e':x) = [(Boolz True, x)]
  readsPrec _ ('t':'r':'u':'e':x) = [(Boolz True, x)]
  readsPrec _ ('f':'a':'l':'s':'e':x) = [(Boolz False, x)]
  readsPrec _ ('F':'a':'l':'s':'e':x) = [(Boolz False, x)]
  readsPrec _ [] = []
  readsPrec n (_:t) = readsPrec n t

makeAbiValue :: AbiType -> String -> AbiValue
makeAbiValue typ str = case readP_to_S (parseAbiValue typ) (padStr str) of
  [(val,"")] -> val
  _ -> error $  "could not parse abi argument: " ++ str ++ " : " ++ show typ
  where
    padStr = case typ of
      (AbiBytesType n) -> padRight' (2 * n + 2) -- +2 is for the 0x prefix
      _ -> id

parseAbiValue :: AbiType -> ReadP AbiValue
parseAbiValue (AbiUIntType n) = do W256 w <- readS_to_P reads
                                   return $ AbiUInt n w
parseAbiValue (AbiIntType n) = do W256 w <- readS_to_P reads
                                  return $ AbiInt n (num w)
parseAbiValue AbiAddressType = AbiAddress <$> readS_to_P reads
parseAbiValue AbiBoolType = (do W256 w <- readS_to_P reads
                                return $ AbiBool (w /= 0))
                            <|> (do Boolz b <- readS_to_P reads
                                    return $ AbiBool b)
parseAbiValue (AbiBytesType n) = AbiBytes n <$> do ByteStringS bytes <- bytesP
                                                   return bytes
parseAbiValue AbiBytesDynamicType = AbiBytesDynamic <$> do ByteStringS bytes <- bytesP
                                                           return bytes
parseAbiValue AbiStringType = AbiString <$> do Char8.pack <$> readS_to_P reads
parseAbiValue (AbiArrayDynamicType typ) =
  AbiArrayDynamic typ <$> do a <- listP (parseAbiValue typ)
                             return $ Vector.fromList a
parseAbiValue (AbiArrayType n typ) =
  AbiArray n typ <$> do a <- listP (parseAbiValue typ)
                        return $ Vector.fromList a
parseAbiValue (AbiTupleType _) = error "tuple types not supported"

listP :: ReadP a -> ReadP [a]
listP parser = between (char '[') (char ']') ((do skipSpaces
                                                  a <- parser
                                                  skipSpaces
                                                  return a) `sepBy` (char ','))

bytesP :: ReadP ByteStringS
bytesP = do
  string "0x"
  hex <- munch isHexDigit
  case BS16.decode (encodeUtf8 (Text.pack hex)) of
    Right d -> pure $ ByteStringS d
    Left d -> pfail

data AbiVals = NoVals | CAbi [AbiValue] | SAbi [SymWord]
  deriving (Show)

decodeBuffer :: [AbiType] -> Buffer -> AbiVals
decodeBuffer tps (ConcreteBuffer b)
  = case runGetOrFail (getAbiSeq (length tps) tps) (BSLazy.fromStrict b) of
      Right ("", _, args) -> CAbi . toList $ args
      _ -> NoVals
decodeBuffer tps b@(SymbolicBuffer _)
  = if containsDynamic tps
    then NoVals
    else SAbi . decodeStaticArgs $ b
  where
    isDynamic t = abiKind t == Dynamic
    containsDynamic = or . fmap isDynamic

decodeStaticArgs :: Buffer -> [SymWord]
decodeStaticArgs buffer = let
    bs = case buffer of
      ConcreteBuffer b -> litBytes b
      SymbolicBuffer b -> b
  in fmap (\i -> S (FromBytes buffer) $
            fromBytes $ take 32 (drop (i*32) bs)) [0..((length bs) `div` 32 - 1)]

-- A modification of 'arbitrarySizedBoundedIntegral' quickcheck library
-- which takes the maxbound explicitly rather than relying on a Bounded instance.
-- Essentially a mix between three types of generators:
-- one that strongly prefers values close to 0, one that prefers values close to max
-- and one that chooses uniformly.
arbitraryIntegralWithMax :: (Integral a) => Integer -> Gen a
arbitraryIntegralWithMax maxbound =
  sized $ \s ->
    do let mn = 0 :: Int
           mx = maxbound
           bits n | n `quot` 2 == 0 = 0
                  | otherwise = 1 + bits (n `quot` 2)
           k  = 2^(s*(bits mn `max` bits mx `max` 40) `div` 100)
       smol <- choose (toInteger mn `max` (-k), toInteger mx `min` k)
       mid <- choose (0, maxbound)
       elements [fromIntegral smol, fromIntegral mid, fromIntegral (maxbound - (fromIntegral smol))]
