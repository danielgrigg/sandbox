import qualified Data.Map.Strict as M
import Data.Bits

encodings = M.fromList $
              zip [0..63] $ 
                ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ ['+','/']

encodeValue k = M.lookup k encodings

  
rint2bits 0 = [0]
rint2bits 1 = [1]
rint2bits n = (n .&. 1) : (rint2bits (n `shiftR` 1))

int2bits :: Int -> [Int]
int2bits = reverse . rint2bits 
