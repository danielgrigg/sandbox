import qualified Data.Map.Strict as M
import Data.Bits
import Data.Char
import Data.List.Split

encodings = M.fromList $
              zip [0..63] $ 
                ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ['+','/']

encodeValue k = M.lookup k encodings

int2bits n = [ testBit n i | i <- [7,6..0]]

bits2int bs = 
  let maxBit = length bs - 1
      bitIndex = zip bs [maxBit,maxBit-1..0]
  in sum $ 
      map (\(isSet,i) -> if isSet then bit i else 0) bitIndex
       

char2bits = int2bits . ord
string2bits s = foldl (++) [] $ map char2bits s

