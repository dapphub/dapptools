module EVM.RLP where

import Prelude hiding (drop, head, length)

-- import EVM.Concrete
import EVM.Types

import Data.ByteString (ByteString, drop, head, length)
import qualified Data.ByteString   as BS


data RLP = BS ByteString | List [RLP] deriving Eq

instance Show RLP where
  show (BS str) = show (ByteStringS str)
  show (List list) = show list

slice :: Int -> Int -> ByteString -> ByteString
slice offset size bs = BS.take size $ BS.drop offset bs

-- helper function returning (the length of the prefix, the length of the content, isList boolean, optimal boolean)
itemInfo :: ByteString -> (Int, Int, Bool, Bool)
itemInfo bs | bs == mempty = (0, 0, False, False)
            | otherwise = case head bs of
  x | 0 <= x && x < 128   -> (0, 1, False, True) -- directly encoded byte
  x | 128 <= x && x < 184 -> (1, num x - 128, False, (length bs /= 2) || (127 < (head $ drop 1 bs))) -- short string
  x | 184 <= x && x < 192 -> (1 + pre, len, False, (len > 55) && head (drop 1 bs) /= 0) -- long string
    where pre = num $ x - 183
          len = num $ word $ slice 1 pre bs
  x | 192 <= x && x < 248 -> (1, num $ x - 192, True, True) -- short list
  x                       -> (1 + pre, len, True, (len > 55) && head (drop 1 bs) /= 0) -- long list
    where pre = num $ x - 247
          len = num $ word $ slice 1 pre bs

rlpdecode :: ByteString -> Maybe RLP
rlpdecode bs | optimal && pre + len == length bs = if isList
                                                   then do
                                                      items <- sequence $
                                                        fmap (\(s, e) -> rlpdecode $ slice s e content) $
                                                        rlplengths content 0 $ len
                                                      Just (List items)
                                                   else Just (BS content)
             | otherwise = Nothing
  where (pre, len, isList, optimal) = itemInfo bs
        content = drop pre bs

rlplengths :: ByteString -> Int -> Int -> [(Int,Int)]
rlplengths bs acc top | acc < top = let (pre, len, _, _) = itemInfo bs
                                    in (acc, pre + len) : rlplengths (drop (pre + len) bs) (acc + pre + len) top
                      | otherwise = []

rlpencode :: RLP -> ByteString
rlpencode (BS bs) = if length bs == 1 && head bs < 128 then bs
                    else encodeLen 128 bs
rlpencode (List items) = encodeLen 192 (mconcat $ map rlpencode items)

encodeLen :: Int -> ByteString -> ByteString
encodeLen offset bs | length bs <= 55 = prefix (length bs) <> bs
                    | otherwise = prefix lenLen <> lenBytes <> bs
          where
            lenBytes = asBE $ length bs
            prefix n = BS.singleton $ num $ offset + n
            lenLen = length lenBytes + 55

rlpList :: [ByteString] -> ByteString
rlpList n = rlpencode $ List $ fmap BS n

rlpWord256 :: W256 -> ByteString
rlpWord256 n = rlpencode $ BS $ word256Bytes n

rlpWord160 :: Addr -> ByteString
rlpWord160 n = rlpencode $ BS $ word160Bytes n
