{-# Language NamedFieldPuns #-}
{-# Language DataKinds #-}
{-# Language OverloadedStrings #-}
{-# Language TypeApplications #-}

module EVM.Symbolic where

import Prelude hiding  (Word)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Control.Lens hiding (op, (:<), (|>), (.>))
import Data.Maybe                   (fromMaybe, fromJust)
import EVM.Types
import EVM.Concrete (Word (..), Whiff(..))
import qualified EVM.Concrete as Concrete
import Data.SBV hiding (runSMT, newArray_, addAxiom, Word)
import qualified Data.SBV.List as SL
import Data.SBV.List ((.++), (.!!))

-- | Symbolic words of 256 bits, possibly annotated with additional
--   "insightful" information
data SymWord = S Whiff (SWord 256)

-- | Convenience functions transporting between the concrete and symbolic realm
sw256 :: SWord 256 -> SymWord
sw256 = S Dull

litWord :: Word -> (SymWord)
litWord (C whiff a) = S whiff (literal $ toSizzle a)

w256lit :: W256 -> SymWord
w256lit = S Dull . literal . toSizzle

litAddr :: Addr -> SAddr
litAddr = SAddr . literal . toSizzle

maybeLitWord :: SymWord -> Maybe Word
maybeLitWord (S whiff a) = fmap (C whiff . fromSizzle) (unliteral a)

maybeLitAddr :: SAddr -> Maybe Addr
maybeLitAddr (SAddr a) = fmap fromSizzle (unliteral a)

maybeLitBytes :: [SWord 8] -> Maybe ByteString
maybeLitBytes xs = fmap (\x -> BS.pack (fmap fromSized x)) (mapM unliteral xs)

-- | Note: these forms are crude and in general,
-- the continuation passing style `forceConcrete`
-- alternatives should be prefered for better error
-- handling when used during EVM execution

forceLit :: SymWord -> Word
forceLit (S whiff a) = case unliteral a of
  Just c -> C whiff (fromSizzle c)
  Nothing -> error "unexpected symbolic argument"

forceLitBytes :: [SWord 8] -> ByteString
forceLitBytes = BS.pack . fmap (fromSized . fromJust . unliteral)

forceBuffer :: Buffer -> ByteString
forceBuffer (ConcreteBuffer b) = b
forceBuffer (SymbolicBuffer b) = forceLitBytes b

-- | Arithmetic operations on SymWord

sdiv :: SymWord -> SymWord -> SymWord
sdiv (S _ x) (S _ y) = let sx, sy :: SInt 256
                           sx = sFromIntegral x
                           sy = sFromIntegral y
                       in sw256 $ sFromIntegral (sx `sQuot` sy)

smod :: SymWord -> SymWord -> SymWord
smod (S _ x) (S _ y) = let sx, sy :: SInt 256
                           sx = sFromIntegral x
                           sy = sFromIntegral y
                       in sw256 $ ite (y .== 0) 0 (sFromIntegral (sx `sRem` sy))

addmod :: SymWord -> SymWord -> SymWord -> SymWord
addmod (S _ x) (S _ y) (S _ z) = let to512 :: SWord 256 -> SWord 512
                                     to512 = sFromIntegral
                                 in sw256 $ sFromIntegral $ ((to512 x) + (to512 y)) `sMod` (to512 z)

mulmod :: SymWord -> SymWord -> SymWord -> SymWord
mulmod (S _ x) (S _ y) (S _ z) = let to512 :: SWord 256 -> SWord 512
                                     to512 = sFromIntegral
                                 in sw256 $ sFromIntegral $ ((to512 x) * (to512 y)) `sMod` (to512 z)

slt :: SymWord -> SymWord -> SymWord
slt (S _ x) (S _ y) =
  sw256 $ ite (sFromIntegral x .< (sFromIntegral y :: (SInt 256))) 1 0

sgt :: SymWord -> SymWord -> SymWord
sgt (S _ x) (S _ y) =
  sw256 $ ite (sFromIntegral x .> (sFromIntegral y :: (SInt 256))) 1 0

shiftRight' :: SymWord -> SymWord -> SymWord
shiftRight' (S _ a') b@(S _ b') = case (num <$> unliteral a', b) of
  (Just n, (S (FromBytes (SymbolicBuffer a)) _)) | n `mod` 8 == 0 && n <= 256 ->
    let bs = replicate (n `div` 8) 0 <> (take ((256 - n) `div` 8) a)
    in S (FromBytes (SymbolicBuffer bs)) (fromBytes bs)
  _ -> sw256 $ sShiftRight b' a'

-- | Operations over static symbolic memory (list of symbolic bytes)
truncpad :: Int -> [SWord 8] -> [SWord 8]
truncpad n xs = if m > n then take n xs
                else mappend xs (replicate (n - m) 0)
  where m = length xs

-- | Is the list concretely known empty?
isConcretelyEmpty :: SymVal a => SList a -> Bool
isConcretelyEmpty sl | Just l <- unliteral sl = null l
                     | True                   = False

-- WARNING: only works when (n <= list length)
takeStatic :: (SymVal a) => Int -> SList a -> [SBV a]
takeStatic 0 ls = []
takeStatic n ls = 
  let (x, xs) = SL.uncons ls
  in x:(takeStatic (n - 1) xs)

-- tries to create a static list whenever possible
dropS :: SymWord -> SList (WordN 8) -> Buffer
dropS n@(S _ i) ls =
  if isConcretelyEmpty ls
  then mempty
  else case (maybeLitWord n, unliteral $ SL.length ls) of
    (Just n', Just l) ->
      if n' == 0
      then StaticSymBuffer $ takeStatic (num $ max n' (num l)) ls
      else let (_, xs) = SL.uncons ls
           in dropS (litWord $ n' - 1) xs
    _ -> DynamicSymBuffer $ SL.drop (sFromIntegral i) ls

-- special case of sliceWithZero when size is known
truncpad' :: Int -> Buffer -> Buffer
truncpad' n m = case m of
  ConcreteBuffer xs -> ConcreteBuffer $ Concrete.byteStringSliceWithDefaultZeroes 0 n xs
  StaticSymBuffer xs -> StaticSymBuffer $ truncpad n xs
  DynamicSymBuffer xs ->
    case unliteral $ SL.length xs of

      Just (num -> l) -> StaticSymBuffer $

        if n <= l
        then takeStatic n xs
        else takeStatic n (xs .++ literal (replicate (n - l) 0))

      Nothing -> StaticSymBuffer $ takeStatic n (xs .++ literal (replicate n 0))

swordAt :: Int -> [SWord 8] -> SymWord
swordAt i bs = sw256 . fromBytes $ truncpad 32 $ drop i bs

swordAt' :: SWord 32 -> SList (WordN 8) -> SymWord
swordAt' i bs = case truncpad' 32 $ dropS (sw256 $ sFromIntegral i) bs of
  ConcreteBuffer s -> litWord $ Concrete.w256 $ Concrete.wordAt 0 s
  StaticSymBuffer s -> sw256 $ fromBytes s
  DynamicSymBuffer s -> sw256 $ fromBytes [s .!! literal i | i <- [0..31]]

readByteOrZero' :: Int -> [SWord 8] -> SWord 8
readByteOrZero' i bs = fromMaybe 0 (bs ^? ix i)

sliceWithZero' :: Int -> Int -> [SWord 8] -> [SWord 8]
sliceWithZero' o s m = truncpad s $ drop o m

writeMemory' :: [SWord 8] -> Word -> Word -> Word -> [SWord 8] -> [SWord 8]
writeMemory' bs1 (C _ n) (C _ src) (C _ dst) bs0 =
  let
    (a, b) = splitAt (num dst) bs0
    a'     = replicate (num dst - length a) 0
    c      = if src > num (length bs1)
             then replicate (num n) 0
             else sliceWithZero' (num src) (num n) bs1
    b'     = drop (num n) b
  in
    a <> a' <> c <> b'

readMemoryWord' :: Word -> [SWord 8] -> SymWord
readMemoryWord' (C _ i) m = sw256 $ fromBytes $ truncpad 32 (drop (num i) m)

readMemoryWord32' :: Word -> [SWord 8] -> SWord 32
readMemoryWord32' (C _ i) m = fromBytes $ truncpad 4 (drop (num i) m)

setMemoryWord' :: Word -> SymWord -> [SWord 8] -> [SWord 8]
setMemoryWord' (C _ i) (S _ x) =
  writeMemory' (toBytes x) 32 0 (num i)

setMemoryByte' :: Word -> SWord 8 -> [SWord 8] -> [SWord 8]
setMemoryByte' (C _ i) x =
  writeMemory' [x] 1 0 (num i)

setMemoryByte'' :: SymWord -> SWord 8 -> Buffer -> Buffer
setMemoryByte'' i x = dynWriteMemory (StaticSymBuffer [x]) 1 0 i

readSWord' :: Word -> [SWord 8] -> SymWord
readSWord' (C _ i) x =
  if i > num (length x)
  then sw256 $ 0
  else swordAt (num i) x

-- | Operations over dynamic symbolic memory (smt list of bytes)
readByteOrZero'' :: SWord 32 -> SList (WordN 8) -> SWord 8
readByteOrZero'' i bs =
  ite (SL.length bs .> (sFromIntegral i + 1))
  (bs .!! (sFromIntegral i))
  (literal 0)


-- Generates a ridiculously large set of constraints (roughly 25k) when
-- the index is symbolic, but it still seems (kind of) manageable
-- for the solvers.
readSWordWithBound :: SWord 32 -> Buffer -> SWord 32 -> SymWord
readSWordWithBound ind (SymbolicBuffer xs) bound = case (num <$> fromSized <$> unliteral ind, num <$> fromSized <$> unliteral bound) of
  (Just i, Just b) ->
    let bs = truncpad 32 $ drop i (take b xs)
    in S (FromBytes (SymbolicBuffer bs)) (fromBytes bs)
  _ -> 
    let boundedList = [ite (i .<= bound) x 0 | (x, i) <- zip xs [1..]]
    in sw256 . fromBytes $ [select' boundedList 0 (ind + j) | j <- [0..31]]

readSWordWithBound ind (ConcreteBuffer xs) bound =
  case fromSized <$> unliteral ind of
    Nothing -> readSWordWithBound ind (SymbolicBuffer (litBytes xs)) bound
    Just x' ->                                       
       -- INVARIANT: bound should always be length xs for concrete bytes
       -- so we should be able to safely ignore it here
         litWord $ Concrete.readMemoryWord (num x') xs

readMemoryWord' :: Word -> [SWord 8] -> SymWord
readMemoryWord' (C _ i) m = sw256 $ fromBytes $ truncpad 32 (drop (num i) m)

dynWriteMemory :: Buffer -> SymWord -> SymWord -> SymWord -> Buffer -> Buffer
dynWriteMemory bs1 n@(S _ n') src@(S _ src') dst@(S _ dst') bs0 =
  let
    a       = sliceWithZero 0 dst bs0
    b       = sliceWithZero src n bs1
    c       = ditchS (sFromIntegral $ dst' + n') bs0

  in
    a <> b <> c

setMemoryWord'' :: SWord 32 -> SymWord -> Buffer -> Buffer
setMemoryWord'' i (S _ x) =
  dynWriteMemory (StaticSymBuffer $ toBytes x) 32 0 (sw256 (sFromIntegral i))

readSWord'' :: SymWord -> SList (WordN 8) -> SymWord
readSWord'' (S _ i) x =
  ite (sFromIntegral i .> SL.length x)
  0
  (swordAt' (sFromIntegral i) x)

-- a whole foldable instance seems overkill, but length is always good to have!
len :: Buffer -> SWord 32
len (DynamicSymBuffer a) = sFromIntegral $ SL.length a
len (StaticSymBuffer bs) = literal . num $ length bs
len (ConcreteBuffer bs) = literal . num $ BS.length bs

grab :: Int -> Buffer -> Buffer
grab n (StaticSymBuffer bs) = StaticSymBuffer $ take n bs
grab n (ConcreteBuffer bs) = ConcreteBuffer $ BS.take n bs
grab n (DynamicSymBuffer bs) =
  case unliteral $ SL.length bs of
    Nothing -> DynamicSymBuffer $ SL.take (literal $ num n) bs
    Just l' -> StaticSymBuffer $ takeStatic (num $ max n (num l')) bs

ditch :: Int -> Buffer -> Buffer
ditch n (StaticSymBuffer bs) = StaticSymBuffer $ drop n bs
ditch n (ConcreteBuffer bs) = ConcreteBuffer $ BS.drop n bs
ditch n (DynamicSymBuffer bs) = dropS (litWord $ num n) bs

ditchS :: SInteger -> Buffer -> Buffer
ditchS n bs = case unliteral n of
  Nothing -> dropS (sw256 $ sFromIntegral n) (dynamize bs)
  Just n' -> ditch (num n') bs

grabS :: SInteger -> Buffer -> Buffer
grabS n bs = case unliteral n of
  Nothing -> DynamicSymBuffer $ SL.take n (dynamize bs)
  Just n' -> grab (num n') bs

readByteOrZero :: Int -> Buffer -> SWord 8
readByteOrZero i (StaticSymBuffer bs) = readByteOrZero' i bs
readByteOrZero i (ConcreteBuffer bs) = num $ Concrete.readByteOrZero i bs
readByteOrZero i (DynamicSymBuffer bs) = readByteOrZero'' (literal $ num i) bs

-- pad up to 10000 bytes in the dynamic case
sliceWithZero :: SymWord -> SymWord -> Buffer -> Buffer
sliceWithZero (S _ o) (S _ s) bf = case (unliteral o, unliteral s, bf) of
  (Just o', Just s', StaticSymBuffer m) -> StaticSymBuffer (sliceWithZero' (num o') (num s') m)
  (Just o', Just s', ConcreteBuffer m)  -> ConcreteBuffer (Concrete.byteStringSliceWithDefaultZeroes (num o') (num s') m)
  (Just o', Just s', m)                 -> truncpad' (num s') (ditch (num o') m)
  _                                     -> DynamicSymBuffer $ SL.subList (dynamize bf .++ literal (replicate 10000 0)) (sFromIntegral o) (sFromIntegral s)

writeMemory :: Buffer -> SymWord -> SymWord -> SymWord -> Buffer -> Buffer
writeMemory bs1 n src dst bs0 =
  case (maybeLitWord n, maybeLitWord src, maybeLitWord dst, bs0, bs1) of
    (Just n', Just src', Just dst', ConcreteBuffer bs0', ConcreteBuffer bs1') ->
      ConcreteBuffer $ Concrete.writeMemory bs1' n' src' dst' bs0'
    (Just n', Just src', Just dst', StaticSymBuffer bs0', ConcreteBuffer bs1') ->
      StaticSymBuffer $ writeMemory' (litBytes bs1') n' src' dst' bs0'
    (Just n', Just src', Just dst', ConcreteBuffer bs0', StaticSymBuffer bs1') ->
      StaticSymBuffer $ writeMemory' bs1' n' src' dst' (litBytes bs0')
    (Just n', Just src', Just dst', StaticSymBuffer bs0', StaticSymBuffer bs1') ->
      StaticSymBuffer $ writeMemory' bs1' n' src' dst' bs0'
    _ -> dynWriteMemory bs1 n src dst bs0

readMemoryWord :: SWord 32 -> Buffer -> SymWord
readMemoryWord i bf = case (unliteral i, bf) of
  (Just i', StaticSymBuffer m) -> readMemoryWord' (num i') m
  (Just i',  ConcreteBuffer m) -> litWord $ Concrete.readMemoryWord (num i') m
  _ -> swordAt' i (dynamize bf)

readMemoryWord32 :: SymWord -> Buffer -> SWord 32
readMemoryWord32 i m = case (maybeLitWord i, m) of
  (Just i', StaticSymBuffer m') -> readMemoryWord32' i' m'
  (Just i', ConcreteBuffer m') -> num $ Concrete.readMemoryWord32 i' m'
  (_, DynamicSymBuffer m') -> case truncpad' 4 $ dropS i m' of
    ConcreteBuffer s -> literal $ num $ Concrete.readMemoryWord32 0 s
    StaticSymBuffer s -> readMemoryWord32' 0 s
    DynamicSymBuffer s -> fromBytes [s .!! literal k | k <- [0..3]]
    

setMemoryWord :: SWord 32 -> SymWord -> Buffer -> Buffer
setMemoryWord i x bf = case (unliteral i, maybeLitWord x, bf) of
  (Just i', Just x', ConcreteBuffer z)  -> ConcreteBuffer $ Concrete.setMemoryWord (num i') x' z
  (Just i', _      , ConcreteBuffer z)  -> StaticSymBuffer $ setMemoryWord' (num i') x (litBytes z)
  (Just i', _      , StaticSymBuffer z) -> StaticSymBuffer $ setMemoryWord' (num i') x z
  _ -> setMemoryWord'' i x bf

setMemoryByte :: SymWord -> SWord 8 -> Buffer -> Buffer
setMemoryByte i x m = case (maybeLitWord i, m) of
  (Just i', StaticSymBuffer m) -> StaticSymBuffer $ setMemoryByte' i' x m
  (Just i', ConcreteBuffer m) -> case fromSized <$> unliteral x of
    Nothing -> StaticSymBuffer $ setMemoryByte' i' x (litBytes m)
    Just x' -> ConcreteBuffer $ Concrete.setMemoryByte i' x' m
  _ -> setMemoryByte'' i x m

readSWord :: SymWord -> Buffer -> SymWord
readSWord i bf = case (maybeLitWord i, bf) of
  (Just i', StaticSymBuffer x) -> readSWord' i' x
  (Just i', ConcreteBuffer x) -> num $ Concrete.readMemoryWord i' x
  _  -> readSWord'' i (dynamize bf)

-- | Custom instances for SymWord, many of which have direct
-- analogues for concrete words defined in Concrete.hs

instance Show SymWord where
  show s@(S Dull _) = case maybeLitWord s of
    Nothing -> "<symbolic>"
    Just w  -> show w
  show (S (Var var) x) = var ++ ": " ++ show x
  show (S (InfixBinOp symbol x y) z) = show x ++ symbol ++ show y  ++ ": " ++ show z
  show (S (BinOp symbol x y) z) = symbol ++ show x ++ show y  ++ ": " ++ show z
  show (S (UnOp symbol x) z) = symbol ++ show x ++ ": " ++ show z
  show (S whiff x) = show whiff ++ ": " ++ show x

instance EqSymbolic SymWord where
  (.==) (S _ x) (S _ y) = x .== y

instance Num SymWord where
  (S _ x) + (S _ y) = sw256 (x + y)
  (S _ x) * (S _ y) = sw256 (x * y)
  abs (S _ x) = sw256 (abs x)
  signum (S _ x) = sw256 (signum x)
  fromInteger x = sw256 (fromInteger x)
  negate (S _ x) = sw256 (negate x)

instance Bits SymWord where
  (S _ x) .&. (S _ y) = sw256 (x .&. y)
  (S _ x) .|. (S _ y) = sw256 (x .|. y)
  (S _ x) `xor` (S _ y) = sw256 (x `xor` y)
  complement (S _ x) = sw256 (complement x)
  shift (S _ x) i = sw256 (shift x i)
  rotate (S _ x) i = sw256 (rotate x i)
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
  symbolicMerge a b (S _ x) (S _ y) = sw256 $ symbolicMerge a b x y
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
