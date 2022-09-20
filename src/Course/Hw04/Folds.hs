module Course.Hw04.Folds (fun1, fun1', fun2, fun2', foldTree, map', xor) where

import Data.List (foldl')

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs


fun1' :: [Integer] -> Integer
fun1' = product . map (2 -) . filter even

-- >>> fun1 [1, 2, 3, 4, 5, 6] == fun1' [1, 2, 3, 4, 5, 6]
-- True

-- >>> fun1 [3, 3, 9, 9, 2] == fun1' [3, 3, 9, 9, 2]
-- True


fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate generator
  where
    generator n = if even n then n `div` 2 else 3 * n + 1

-- 7
-- fun2 (22)
-- 22 + fun2 (11)
-- 22 + fun2 (34)
-- 22 + 34 + fun2 (17)
-- 22 + 34 + fun2 (52)
-- 22 + 34 + 52 + fun2 (26)
-- 22 + 34 + 52 + 26 + fun2 (13)
-- 22 + 34 + 52 + 26 + fun2 (40)
-- 22 + 34 + 52 + 26 + 40 + fun2 (20)
-- 22 + 34 + 52 + 26 + 40 + 20 + fun2 (10)
-- 22 + 34 + 52 + 26 + 40 + 20 + 10 + fun2 (5)
-- 22 + 34 + 52 + 26 + 40 + 20 + 10 + fun2 (16)
-- 22 + 34 + 52 + 26 + 40 + 20 + 10 + 16 + fun2 (8)
-- 22 + 34 + 52 + 26 + 40 + 20 + 10 + 16 + 8 + fun2 (4)
-- 22 + 34 + 52 + 26 + 40 + 20 + 10 + 16 + 8 + 4 + fun2 (2)
-- 22 + 34 + 52 + 26 + 40 + 20 + 10 + 16 + 8 + 4 + 2 + fun2 (1)

-- >>> fun2 1 == fun2' 1
-- True

-- >>> fun2 2 == fun2' 2
-- True

-- >>> fun2 4 == fun2' 4
-- True

-- >>> fun2 7 == fun2' 7
-- True

-- >>> fun2 10 == fun2' 10
-- True


data Tree a
  = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr ins Leaf
  where
    ins e tree = case tree of
      Leaf -> Node 0 Leaf e Leaf
      Node _ left a right -> case compare leftHeigth rightHeigth of
        GT -> Node (leftHeigth + 1) left a (ins e right)
        _ -> Node (rightHeigth + 1) (ins e left) a right
        where
          leftHeigth = heigth left
          rightHeigth = heigth right
          heigth n = case n of
            Leaf -> 0
            Node h' _ _ _ -> h'

-- >>> foldTree "ABCDEFGHIJ"

-- Node 3
--   (Node 2
--     (Node 1 (Node 0 Leaf 'E' Leaf) 'H' Leaf )
--     'I'
--     (Node 1 (Node 0 Leaf 'A' Leaf) 'D' Leaf ))
--   'J'
--   (Node 2
--     (Node 1 (Node 0 Leaf 'C' Leaf) 'F' Leaf )
--     'G'
--     (Node 0 Leaf 'B' Leaf))


xor :: [Bool] -> Bool
xor = foldl' (\ a b -> (a || b) && not (a && b)) False

-- >>> xor [False, True, False] == True
-- True

-- >>> xor [False, True, False, False, True] == False
-- True


map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\ e acc -> (f e) : acc) []

-- >>> map (+1) [0, 1, 2, 3] == map' (+1) [0, 1, 2, 3]
-- True
