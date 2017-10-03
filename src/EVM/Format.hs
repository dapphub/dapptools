module EVM.Format where

import Prelude hiding (Word)

import EVM (cheatCode)
import EVM.Concrete (Concrete, Word (..))
import EVM.Types (W256 (..), num)
import EVM.ABI (AbiValue (..))

import Control.Arrow ((>>>))
import Data.ByteString (ByteString)
import Data.ByteString.Builder    (byteStringHex, toLazyByteString)
import Data.ByteString.Lazy (toStrict)
import Data.DoubleWord (signedWord)
import Data.Foldable (toList)
import Data.Monoid ((<>))
import Data.Text (Text, pack, intercalate)
import Data.Text.Encoding (decodeUtf8, decodeUtf8')
import Data.Vector (Vector)

import Numeric (showHex)

import qualified Data.Char as Char
import qualified Data.Scientific as Scientific
import qualified Data.Text as Text

data Signedness = Signed | Unsigned

showDec :: Signedness -> W256 -> Text
showDec signed (W256 w) =
  let
    i = case signed of
          Signed   -> num (signedWord w)
          Unsigned -> num w
          
  in
    if i == num cheatCode
    then "<hevm cheat address>"
    else
      if abs i > 1000000000000
      then
        "~" <> pack (Scientific.formatScientific
                       Scientific.Generic
                       (Just 8)
                       (fromIntegral i))
      else
        showDecExact i

showDecExact :: Integer -> Text
showDecExact = humanizeInteger

showWordExact :: Word Concrete -> Text
showWordExact (C _ (W256 w)) = humanizeInteger w

humanizeInteger :: (Num a, Integral a, Show a) => a -> Text
humanizeInteger =
  ( Text.intercalate ","
  . reverse
  . map Text.reverse
  . Text.chunksOf 3
  . Text.reverse
  . Text.pack
  . show
  )

-- TODO: make polymorphic
showAbiValues :: Vector AbiValue -> Text
showAbiValues vs =
  "(" <> intercalate ", " (toList (fmap showAbiValue vs)) <> ")"

showAbiValue :: AbiValue -> Text
showAbiValue (AbiUInt _ w)        = humanizeInteger w
showAbiValue (AbiInt _ w)         = humanizeInteger w
showAbiValue (AbiAddress w160)    = pack $ "0x" ++ (showHex w160 "")
showAbiValue (AbiBool b)          = pack $ show b
showAbiValue (AbiBytes _ bs)      = formatBytes bs
showAbiValue (AbiBytesDynamic bs) = formatBinary bs
showAbiValue (AbiString bs)       = formatQString bs
-- TODO: arrays
showAbiValue value = pack $ show value

isPrintable :: ByteString -> Bool
isPrintable =
  decodeUtf8' >>>
    either (const False) 
      (Text.all (not . Char.isControl))

formatBytes :: ByteString -> Text
formatBytes b | isPrintable b = formatQString b
              | otherwise     = formatBinary b

formatQString :: ByteString -> Text
formatQString = pack . show

formatString :: ByteString -> Text
formatString = decodeUtf8

formatBinary :: ByteString -> Text
formatBinary =
  (<>) "0x" . decodeUtf8 . toStrict . toLazyByteString . byteStringHex
