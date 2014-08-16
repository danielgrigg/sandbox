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

englishLetterFreq = 
  [0.08167, 0.01492, 0.02782, 0.04253, 0.130001, 0.02228, 0.02015, 0.06094, 
   0.06966, 0.00153, 0.00772, 0.04025, 0.02406, 0.06749, 0.07507, 0.01929, 
   0.00095, 0.05987, 0.06327, 0.09056, 0.02758, 0.00978, 0.0236, 0.0015, 
   0.01974, 0.00074]

addLetter hist c = 
  let oldFreq = (M.findWithDefault 0 c hist)
  in M.insert c (1 + oldFreq ) hist

emptyLetterFreq = 
  let letters = ['a'..'z']
  in M.fromList $ zip letters $ replicate (length letters) 0

histogram s = foldl addLetter emptyLetterFreq s

freqs s = 
  let hist = histogram s
      count = sum $ M.elems hist
      countFloat = (fromIntegral count)::Double
  in M.map (\a -> (fromIntegral a) / countFloat) hist

sumSquaresError [] _ = 0.0
sumSquaresError _ [] = 0.0
sumSquaresError xs ys =
  let zs = zip xs ys
      sumErrors =  sum $ map (\(x,y) -> (x - y)^2) $ zs
      n = length zs
  in  sumErrors / (fromIntegral n)::Double


simplifyText s = 
  filter (\c -> c >= 'a' && c <= 'z') $
  map toLower s

score text =
  let soup = simplifyText text 
      soupFreqs = M.elems $ freqs soup
  in sumSquaresError soupFreqs englishLetterFreq

corpus0 = "It was Easter last year. Mum and I were at a Vietnamese restaurant in Sydney. At 27, I was vaguely aware that there was no longer infinite time left for me to have children. I loved my job as a reporter. I didn't want kids any time soon. But I didn't want all my options to vanish while I was busy filing stories. So sitting there, facing her, I asked the question."

