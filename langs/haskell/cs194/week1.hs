toDigitsRev :: Integer -> [Integer]
toDigitsRev x 
  | x > 0 = x `mod` 10 : toDigitsRev (x `div` 10)
  | otherwise = []  

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

doubleEveryOther' :: Integer -> [Integer] -> [Integer]
doubleEveryOther' _ [] = []
doubleEveryOther' i (x:xs)
  | even i = x : (doubleEveryOther' (i+1) xs )
  | otherwise = 2*x : (doubleEveryOther' (i+1) xs)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . (doubleEveryOther' 0) . reverse

sumDigit :: Integer -> Integer
sumDigit x = x `div` 10 + x `mod` 10

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sumDigit x + sumDigits xs

sumCCNum = sumDigits . doubleEveryOther . toDigits

validate :: Integer -> Bool
validate n = sumCCNum n `mod` 10 == 0

