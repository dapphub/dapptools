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
import qualified EVM.Concrete as Concrete
import Data.SBV hiding (runSMT, newArray_, addAxiom, Word)

litWord :: Word -> (SymWord)
litWord (C whiff a) = S whiff (literal $ toSizzle a)

w256lit :: W256 -> SymWord
w256lit x = S (Val (show x)) $ literal $ toSizzle x

litAddr :: Addr -> SAddr
litAddr = SAddr . literal . toSizzle

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
iteWhiff :: String -> SBool -> SymWord -> SymWord -> SymWord
iteWhiff symbol cond (S a _) (S b _) =
  ite cond
  (S (InfixBinOp symbol a b) 1) (S (UnOp "not" (InfixBinOp symbol a b)) 0)


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
slt x'@(S _ x) y'@(S _ y) =
  iteWhiff "signed<" (sFromIntegral x .< (sFromIntegral y :: (SInt 256))) x' y'

sgt :: SymWord -> SymWord -> SymWord
sgt x'@(S _ x) y'@(S _ y) =
  iteWhiff "signed>" (sFromIntegral x .> (sFromIntegral y :: (SInt 256))) x' y'

shiftRight' :: SymWord -> SymWord -> SymWord
shiftRight' (S _ a') b@(S _ b') = case (num <$> unliteral a', b) of
  (Just n, (S (FromBytes ind (SymbolicBuffer a)) _)) | n `mod` 8 == 0 && n <= 256 ->
    let bs = replicate (n `div` 8) 0 <> (take ((256 - n) `div` 8) a)
    in S (FromBytes ind (SymbolicBuffer bs)) (fromBytes bs)
  (Just n, (S (FromCalldata ind (SymbolicBuffer a)) _)) | n `mod` 8 == 0 && n <= 256 ->
    let bs = replicate (n `div` 8) 0 <> (take ((256 - n) `div` 8) a)
    in S (FromCalldata ind (SymbolicBuffer bs)) (fromBytes bs)
  _ -> sw256 $ sShiftRight b' a'

-- | Operations over symbolic memory (list of symbolic bytes)
swordAt :: Int -> [SWord 8] -> SymWord
swordAt i bs = sw256 . fromBytes $ truncpad 32 $ drop i bs

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
    b'     = drop (num (n)) b
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

readSWord' :: Word -> [SWord 8] -> SymWord
readSWord' (C _ i) x =
  if i > num (length x)
  then 0
  else swordAt (num i) x


select' :: (Ord b, Num b, SymVal b, Mergeable a) => [a] -> a -> SBV b -> a
select' xs err ind = walk xs ind err
    where walk []     _ acc = acc
          walk (e:es) i acc = walk es (i-1) (ite (i .== 0) e acc)

-- Generates a ridiculously large set of constraints (roughly 25k) when
-- the index is symbolic, but it still seems (kind of) manageable
-- for the solvers.
readSWordWithBound :: SymWord -> Buffer -> SWord 256 -> SymWord
readSWordWithBound sind@(S _ ind) (SymbolicBuffer xs) bound = case (num <$> maybeLitWord sind, num <$> fromSizzle <$> unliteral bound) of
  (Just i, Just b) ->
    let bs = truncpad 32 $ drop i (take b xs)
    in S (FromBytes sind (SymbolicBuffer bs)) (fromBytes bs)
  _ ->
    let boundedList = [ite (i .<= bound) x 0 | (x, i) <- zip xs [1..]]
        res = [select' boundedList 0 (ind + j) | j <- [0..31]]
    in S (FromBytes sind (SymbolicBuffer res)) $ fromBytes $ res

readSWordWithBound sind (ConcreteBuffer xs) bound =
  case maybeLitWord sind of
    Nothing -> readSWordWithBound sind (SymbolicBuffer (litBytes xs)) bound
    Just x' ->
       -- INVARIANT: bound should always be length xs for concrete bytes
       -- so we should be able to safely ignore it here
         litWord $ Concrete.readMemoryWord x' xs

-- a whole foldable instance seems overkill, but length is always good to have!
len :: Buffer -> Int
len (SymbolicBuffer bs) = length bs
len (ConcreteBuffer bs) = BS.length bs

grab :: Int -> Buffer -> Buffer
grab n (SymbolicBuffer bs) = SymbolicBuffer $ take n bs
grab n (ConcreteBuffer bs) = ConcreteBuffer $ BS.take n bs

ditch :: Int -> Buffer -> Buffer
ditch n (SymbolicBuffer bs) = SymbolicBuffer $ drop n bs
ditch n (ConcreteBuffer bs) = ConcreteBuffer $ BS.drop n bs

readByteOrZero :: Int -> Buffer -> SWord 8
readByteOrZero i (SymbolicBuffer bs) = readByteOrZero' i bs
readByteOrZero i (ConcreteBuffer bs) = num $ Concrete.readByteOrZero i bs

sliceWithZero :: Int -> Int -> Buffer -> Buffer
sliceWithZero o s (SymbolicBuffer m) = SymbolicBuffer (sliceWithZero' o s m)
sliceWithZero o s (ConcreteBuffer m) = ConcreteBuffer (Concrete.byteStringSliceWithDefaultZeroes o s m)

writeMemory :: Buffer -> Word -> Word -> Word -> Buffer -> Buffer
writeMemory (ConcreteBuffer bs1) n src dst (ConcreteBuffer bs0) =
  ConcreteBuffer (Concrete.writeMemory bs1 n src dst bs0)
writeMemory (ConcreteBuffer bs1) n src dst (SymbolicBuffer bs0) =
  SymbolicBuffer (writeMemory' (litBytes bs1) n src dst bs0)
writeMemory (SymbolicBuffer bs1) n src dst (ConcreteBuffer bs0) =
  SymbolicBuffer (writeMemory' bs1 n src dst (litBytes bs0))
writeMemory (SymbolicBuffer bs1) n src dst (SymbolicBuffer bs0) =
  SymbolicBuffer (writeMemory' bs1 n src dst bs0)

readMemoryWord :: Word -> Buffer -> SymWord
readMemoryWord i (SymbolicBuffer m) = readMemoryWord' i m
readMemoryWord i (ConcreteBuffer m) = litWord $ Concrete.readMemoryWord i m

readMemoryWord32 :: Word -> Buffer -> SWord 32
readMemoryWord32 i (SymbolicBuffer m) = readMemoryWord32' i m
readMemoryWord32 i (ConcreteBuffer m) = num $ Concrete.readMemoryWord32 i m

setMemoryWord :: Word -> SymWord -> Buffer -> Buffer
setMemoryWord i x (SymbolicBuffer z) = SymbolicBuffer $ setMemoryWord' i x z
setMemoryWord i x (ConcreteBuffer z) = case maybeLitWord x of
  Just x' -> ConcreteBuffer $ Concrete.setMemoryWord i x' z
  Nothing -> SymbolicBuffer $ setMemoryWord' i x (litBytes z)

setMemoryByte :: Word -> SWord 8 -> Buffer -> Buffer
setMemoryByte i x (SymbolicBuffer m) = SymbolicBuffer $ setMemoryByte' i x m
setMemoryByte i x (ConcreteBuffer m) = case fromSized <$> unliteral x of
  Nothing -> SymbolicBuffer $ setMemoryByte' i x (litBytes m)
  Just x' -> ConcreteBuffer $ Concrete.setMemoryByte i x' m

readSWord :: Word -> Buffer -> SymWord
readSWord i (SymbolicBuffer x) = readSWord' i x
readSWord i (ConcreteBuffer x) = num $ Concrete.readMemoryWord i x
