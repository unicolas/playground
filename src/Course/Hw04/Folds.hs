module Course.Hw04.Folds (fun1, fun1', fun2, fun2') where

import Data.List (foldl')

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs


fun1' :: [Integer] -> Integer
fun1' = foldl' (*) 1 . map (2 -) . filter even

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
fun2' = foldl' (+) 0 . filter even . takeWhile (>1) . iterate generator
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
