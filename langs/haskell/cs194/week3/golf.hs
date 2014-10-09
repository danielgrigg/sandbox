module Golf where

import Data.List

every :: [a] -> Int -> [a]
every xs n = case drop (n-1) xs of
  (y:ys) -> y : every ys n
  [] -> []

skips :: [a] -> [[a]]  
skips xs = map (every xs) [1..length xs] 
  
localMaxima :: [Integer] -> [Integer]
localMaxima (a:rest@(b:c:_))
  | b > a && b > c = b : localMaxima rest
  | otherwise = localMaxima rest
localMaxima _ = []

--histogram :: [Integer] -> String
histo xs =
  map (\i -> length $ filter (== i) xs) [1..9]


histogram xs =
  let hs = histo xs
      go _ 0 = "==========\n0123456789"
      go ys n = map (\x -> if x >= n then '*' else ' ') ys ++ "\n" ++ go ys (n-1)
  in go hs (maximum hs)
