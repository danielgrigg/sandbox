import Data.Char
import Data.List.Split
import qualified Data.Map.Strict as M
import Data.Bits
import Data.Maybe

-- base64 encoding table
encodings = M.fromList $
              zip [0..63] $
                ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ['+','/']

-- encode a 6-bit value in base64
encodeValue k = M.lookup k encodings

-- convert a 8-bit value to a list of bit flags
bool2bin False = 0
bool2bin True = 1
byte2bits n = [bool2bin $ testBit n i | i <- [0..7]]

-- convert a list of bit flags to an integer value
bits2int::[Int] -> Int
bits2int bs =
  let maxBit = length bs - 1
      bitIndex = zip bs [0..maxBit] --[maxBit,maxBit-1..0]
  in sum $
      map (\(b,i) -> if (b == 1) then bit i else 0) bitIndex

-- read hex cause we don't have numeric
readHex (a:b:[]) = (digitToInt a) * 16 + digitToInt b
byteValues s = map readHex $ chunksOf 2 s
bitstream s = foldl (\a b -> (byte2bits b) ++ a ) [] $ byteValues s

hex2base64 s = 
  reverse $
  map (fromJust . encodeValue . bits2int) $ 
  chunksOf 6 $ 
  bitstream s


