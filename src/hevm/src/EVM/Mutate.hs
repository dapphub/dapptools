{-
   Mutators for abi values, adapted from echidna
-}
module EVM.Mutate where

import Data.Bifunctor (second)
import Data.Bool (bool)
import Test.QuickCheck.Gen (Gen, chooseInt, chooseInteger, frequency)
import Data.DoubleWord (Int256, Word256)
import Test.QuickCheck.Arbitrary (arbitrary)

import EVM.ABI

import Control.Monad (replicateM)
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import qualified Data.ListLike as LL

-- | Given an 'AbiValue', generate a random \"similar\" value of the same 'AbiType'.
mutateAbiValue :: AbiValue -> Gen AbiValue
mutateAbiValue (AbiUInt n x)         = chooseInt (0, 9) >>= -- 10% of chance of mutation
                                          \case
                                            0 -> fixAbiUInt n <$> mutateNum x
                                            _ -> return $ AbiUInt n x
mutateAbiValue (AbiInt n x)          = chooseInt (0, 9) >>= -- 10% of chance of mutation
                                          \case
                                            0 -> fixAbiInt n <$> mutateNum x
                                            _ -> return $ AbiInt n x

mutateAbiValue (AbiAddress x)        = return $ AbiAddress x
mutateAbiValue (AbiBool _)           = genAbiValue AbiBoolType
mutateAbiValue (AbiBytes n b)        = do fs <- replicateM n arbitrary
                                          xs <- mutateLL (Just n) (BS.pack fs) b
                                          return (AbiBytes n xs)

mutateAbiValue (AbiBytesDynamic b)   = AbiBytesDynamic <$> mutateLL Nothing mempty b
mutateAbiValue (AbiString b)         = AbiString <$> mutateLL Nothing mempty b
mutateAbiValue (AbiArray n t l)      = do fs <- replicateM n $ genAbiValue t
                                          xs <- mutateLL (Just n) (V.fromList fs) l
                                          return (AbiArray n t xs)

mutateAbiValue (AbiArrayDynamic t l) = AbiArrayDynamic t <$> mutateLL Nothing mempty l
mutateAbiValue (AbiTuple v)          = AbiTuple          <$> traverse mutateAbiValue v


-- | Mutate a list-like data structure using a list of mutators
mutateLL :: LL.ListLike f i
         => Maybe Int -- ^ Required size for the mutated list-like value (or Nothing if there are no constrains)
         -> f         -- ^ Randomly generated list-like value to complement the mutated list, if it is shorter than the requested size
         -> f         -- ^ List-like value to mutate
         -> Gen f
mutateLL mn fs vs = do
  f <- genMutator
  xs <- f vs
  return $ maybe xs (`LL.take` (xs <> fs)) mn

-- | A list of mutators to randomly select to perform a mutation of list-like values
genMutator :: LL.ListLike f i => Gen (f -> Gen f)
genMutator = frequency . fmap (second pure) $ [(1, return), (10, expandRandList), (10, deleteRandList), (10, swapRandList)]

-- |
expandRandList :: LL.ListLike f i => f -> Gen f
expandRandList xs
  | l == 0    = return xs
  | l >= 32   = return xs
  | otherwise = do
    k <- chooseInt (0, l - 1)
    t <- chooseInt (1, min 32 l)
    return $ expandAt xs k t
  where l = LL.length xs

expandAt :: LL.ListLike f i => f -> Int -> Int -> f
expandAt xs k t =
  case LL.uncons xs of
    Nothing     -> xs
    Just (y,ys) -> if k == 0
                   then LL.replicate t y <> ys
                   else LL.cons y (expandAt ys (k - 1) t)

-- | Delete a random element from the `ListLike` f
deleteRandList :: LL.ListLike f i => f -> Gen f
deleteRandList xs =
  if LL.null xs
  then return xs
  else do
    k <- chooseInt (0, LL.length xs - 1)
    return $ deleteAt k xs

deleteAt :: LL.ListLike f i => Int -> f -> f
deleteAt n f = LL.take n f <> LL.drop (n+1) f

-- | Given a `ListLike` f, swap two random elements
swapRandList :: LL.ListLike f i => f -> Gen f
swapRandList xs =
  if LL.null xs
  then return xs
  else do
    i <- chooseInt (0, LL.length xs - 1)
    j <- chooseInt (0, LL.length xs - 1)
    return $ if i == j then xs else swapAt xs (min i j) (max i j)

-- taken from https://stackoverflow.com/questions/30551033/swap-two-elements-in-a-list-by-its-indices/30551130#30551130
swapAt :: LL.ListLike f i => f -> Int -> Int -> f
swapAt xs i j = left <> LL.cons elemJ middle <> LL.cons elemI right
  where elemI = xs `LL.index` i
        elemJ = xs `LL.index` j
        left = LL.take i xs
        middle = LL.take (j - i - 1) (LL.drop (i + 1) xs)
        right = LL.drop (j + 1) xs


-- | Given an 'Integral' number n, get a random number in [0,2n].
mutateNum :: Integral a => a -> Gen a
mutateNum x = bool (x +) (x -) <$> arbitrary <*> (fromInteger <$> (chooseInteger (0, toInteger x)))

-- | Force `x` to be in the range of a uint of size `n`
fixAbiUInt :: Int -> Word256 -> AbiValue
fixAbiUInt n x = AbiUInt n (x `mod` ((2 ^ n) - 1))

-- | Force `x` to be in the range of an int of size `n`
fixAbiInt :: Int -> Int256 -> AbiValue
fixAbiInt n x = AbiInt n (x `mod` 2 ^ (n - 1))
