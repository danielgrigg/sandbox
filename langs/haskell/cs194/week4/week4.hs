module Week4 where

import Data.List
--fun1 :: [Integer] -> Integer

fun1 :: [Integer] -> Integer
fun1 = product . map (subtract  2) .  filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)


data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)


depth :: Tree t -> Integer
depth Leaf = 0
depth (Node n _ _ _) = n

treeInsert :: a -> Tree a -> Tree a

treeInsert v Leaf = Node 0 Leaf v Leaf

treeInsert v (Node _ Leaf v' right) =
  Node (depth right + 1) (treeInsert v Leaf) v' right

treeInsert v (Node _ left v' Leaf) =
  Node (depth left + 1) left v' (treeInsert v Leaf)
  
treeInsert v (Node _ left v' right) =
  let leftInsert = treeInsert v left
      rightInsert = treeInsert v right
  in
   if depth left <= depth right
   then Node (depth leftInsert + 1) leftInsert v' right
   else Node (depth rightInsert + 1) left v' rightInsert

foldTree :: [a] -> Tree a
foldTree vs = foldr treeInsert Leaf vs


xor :: [Bool] -> Bool
xor = foldr (\b r -> case (b,r) of
              (False,_) -> r
              (True,False) -> True
              (True,True) -> False) False
 
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base = foldr (flip f) base . reverse


-- given n, generate all odd primes up to 2n + 2
sieveSundaram :: Integer -> [Integer]
sieveSundaram = undefined

cartProd :: [a] -> [b] -> [(a,b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

sieve :: Integer -> [Integer]
sieve n =
  let deleted =
        map (\(i,j) -> i+j+2*i*j)
        . filter (\(i,j) -> i <= j && i+j+2*i*j <= n)
        $ cartProd [1..n] [1..n]
  in map (\x -> 2 * x + 1)
     . filter (\x -> not $ x `elem` deleted)
     $ [1..n]
