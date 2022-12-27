-- Copyright (c) 2011, Galois Inc.  All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
-- 
--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
-- 
--     * Redistributions in binary form must reproduce the above
--       copyright notice, this list of conditions and the following
--       disclaimer in the documentation and/or other materials provided
--       with the distribution.
-- 
--     * Neither the name of Trevor Elliott nor the names of other
--       contributors may be used to endorse or promote products derived
--       from this software without specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
-- OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
-- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--

module EVM.Hexdump (prettyHex, simpleHex) where

import Data.ByteString                       (ByteString)
import qualified Data.ByteString       as B  (length, unpack)
import qualified Data.ByteString.Char8 as B8 (unpack)
import Data.Char                             (isAscii, isControl)
import Data.List                             (intercalate, transpose, unfoldr)
import Numeric                               (showHex)

byteWidth :: Num a => a
byteWidth    = 2  -- Width of an padded 'Word8'

numWordBytes :: Num a => a
numWordBytes = 4  -- Number of bytes to group into a 32-bit word

-- |'prettyHex' renders a 'ByteString' as a multi-line 'String' complete with
-- addressing, hex digits, and ASCII representation.
--
-- Sample output
--
-- @Length: 100 (0x64) bytes
--0000:   4b c1 ad 8a  5b 47 d7 57  48 64 e7 cc  5e b5 2f 6e   K...[G.WHd..^./n
--0010:   c5 b3 a4 73  44 3b 97 53  99 2d 54 e7  1b 2f 91 12   ...sD;.S.-T../..
--0020:   c8 1a ff c4  3b 2b 72 ea  97 e2 9f e2  93 ad 23 79   ....;+r.......#y
--0030:   e8 0f 08 54  02 14 fa 09  f0 2d 34 c9  08 6b e1 64   ...T.....-4..k.d
--0040:   d1 c5 98 7e  d6 a1 98 e2  97 da 46 68  4e 60 11 15   ...~......FhN`..
--0050:   d8 32 c6 0b  70 f5 2e 76  7f 8d f2 3b  ed de 90 c6   .2..p..v...;....
--0060:   93 12 9c e1                                          ....@
prettyHex :: Int -> ByteString -> String
prettyHex hexDisplayWidth bs = unlines (header : body)
 where
  numLineWords    = 4  -- Number of words to group onto a line
  addressWidth    = 4  -- Minimum width of a padded address
  numLineBytes    = numLineWords * numWordBytes -- Number of bytes on a line
  replacementChar = '.' -- 'Char' to use for non-printable characters

  header = "Length: " ++ show    (B.length bs)
        ++ " (0x"     ++ showHex (B.length bs) ") bytes"

  body = map (intercalate "   ")
       $ transpose [mkLineNumbers, mkHexDisplay bs, mkAsciiDump bs]

  mkHexDisplay
    = padLast hexDisplayWidth
    . map (intercalate "  ") . group numLineWords
    . map (intercalate " ")  . group numWordBytes
    . map (paddedShowHex byteWidth)
    . B.unpack

  mkAsciiDump = group numLineBytes . cleanString . B8.unpack

  cleanString = map go
   where
    go x | isWorthPrinting x = x
         | otherwise         = replacementChar

  mkLineNumbers = [paddedShowHex addressWidth (x * numLineBytes) ++ ":"
                   | x <- [0 .. (B.length bs - 1) `div` numLineBytes] ]

  padLast w [x]         = [x ++ replicate (w - length x) ' ']
  padLast w (x:xs)      = x : padLast w xs
  padLast _ []          = []

-- |'paddedShowHex' displays a number in hexidecimal and pads the number
-- with 0 so that it has a minimum length of @w@.
paddedShowHex :: (Show a, Integral a) => Int -> a -> String
paddedShowHex w n = pad ++ str
    where
     str = showHex n ""
     pad = replicate (w - length str) '0'


-- |'simpleHex' converts a 'ByteString' to a 'String' showing the octets
-- grouped in 32-bit words.
--
-- Sample output
--
-- @4b c1 ad 8a  5b 47 d7 57@
simpleHex :: ByteString -> String
simpleHex = intercalate "  "
          . map (intercalate " ") . group numWordBytes
          . map (paddedShowHex byteWidth)
          . B.unpack

-- |'isWorthPrinting' returns 'True' for non-control ascii characters.
-- These characters will all fit in a single character when rendered.
isWorthPrinting :: Char -> Bool
isWorthPrinting x = isAscii x && not (isControl x)

-- |'group' breaks up a list into sublists of size @n@. The last group
-- may be smaller than @n@ elements. When @n@ less not positive the
-- list is returned as one sublist.
group :: Int -> [a] -> [[a]]
group n
 | n <= 0    = (:[])
 | otherwise = unfoldr go
  where
    go [] = Nothing
    go xs = Just (splitAt n xs)
