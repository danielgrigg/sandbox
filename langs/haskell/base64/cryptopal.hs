import Data.Char
import Data.List.Split
import qualified Data.Map.Strict as M
import Data.Bits
import Data.Maybe

replaceLast n r s = (take (length s - n) s) ++ (replicate n r)
bool2bin False = 0
bool2bin True = 1


-- read hex cause we don't have numeric
readHexByte (a:b:[]) = (digitToInt a) * 16 + digitToInt b
showHexByte x = [(intToDigit (div x 16)), (intToDigit (mod x 16))]
byteValues s = map readHexByte $ chunksOf 2 s
bitstream s = foldl (\a b -> (byte2bits b) ++ a ) [] $ byteValues s

-- base64 encoding table
encodings = M.fromList $
              zip [0..63] $
                ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ['+','/']

-- encode a 6-bit value in base64
encodeValue k = M.lookup k encodings

-- convert a 8-bit value to a list of bit flags
byte2bits n = [bool2bin $ testBit n i | i <- [0..7]]

-- convert a list of bit flags to an integer value
bits2int::[Int] -> Int
bits2int bs =
  let maxBit = length bs - 1
      bitIndex = zip bs [0..maxBit] --[maxBit,maxBit-1..0]
  in sum $
      map (\(b,i) -> if (b == 1) then bit i else 0) bitIndex

-- | Base convert hex string to base64
hex2base64 s = 
  reverse $
  map (fromJust . encodeValue . bits2int) $ 
  chunksOf 6 $ 
  bitstream s

char2bits = byte2bits . ord
string2bits s = foldl (++) [] $ map char2bits s

encodePadded s = map (fromJust . encodeValue . bits2int) $ (chunksOf 6) . string2bits $ s
leftover s n = length s `mod` n


-- | Base64 encode a string.
-- TODO - currently broken cause I'm drunk and tried refactoring
-- when the implementations were different between the two source
-- files but whs
encode s | leftover s 3 == 1  = replaceLast 2 '=' $ encodePadded $ s ++ "\0\0"
encode s | leftover s 3 == 2  = replaceLast 1 '=' $ encodePadded $ s ++ "\0"
encode s = encodePadded s

xorBit a b = a `xor` b

xorByteValues a b = map (\(x,y) -> xor x y) $ zip a b

untuple2 f (x,y) = f x y

xorHexString a b = 
  concat $
  map (showHexByte . (untuple2 xor))  $
  zip (byteValues a) (byteValues b)

