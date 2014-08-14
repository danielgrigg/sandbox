import qualified Data.Map.Strict as M
import Data.Bits
import Data.Char
import Data.List.Split
import Data.Maybe

-- base64 encoding table
encodings = M.fromList $
              zip [0..63] $ 
                ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ['+','/']

-- encode a 6-bit value in base64
encodeValue k = M.lookup k encodings

-- convert a 8-bit value to a list of bit flags
byte2bits n = [ testBit n i | i <- [7,6..0]]

-- convert a list of bit flags to an integer value
bits2int::[Bool] -> Int
bits2int bs = 
  let maxBit = length bs - 1
      bitIndex = zip bs [maxBit,maxBit-1..0]
  in sum $ 
      map (\(isSet,i) -> if isSet then bit i else 0) bitIndex
       

char2bits = byte2bits . ord
string2bits s = foldl (++) [] $ map char2bits s

-- replace last n values  with r
replaceLast n r s = (take (length s - n) s) ++ (replicate n r)

encodePadded s = map (fromJust . encodeValue . bits2int) $ (chunksOf 6) . string2bits $ s

leftover s n = length s `mod` n

encode s | leftover s 3 == 1  = replaceLast 2 '=' $ encodePadded $ s ++ "\0\0"
encode s | leftover s 3 == 2  = replaceLast 1 '=' $ encodePadded $ s ++ "\0"
encode s = encodePadded s
