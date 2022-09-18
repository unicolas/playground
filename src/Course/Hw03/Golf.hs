module Course.Hw03.Golf (skips, skips', localMaxima, histogram) where

import Data.List (foldl')

skips :: [a] -> [[a]]
skips l = map (`dropEvery` l) [1..(length l)]

dropEvery :: Int -> [a] -> [a]
dropEvery _ [] = []
dropEvery n l = take 1 xs ++ dropEvery n (drop 1 xs)
  where (_, xs) = splitAt (n - 1) l

-- >>> skips "ABCD" == ["ABCD", "BD", "C", "D"]
-- True

-- >>> skips "hello!" == ["hello!", "el!", "l!", "l", "o", "!"]
-- True

-- >>> skips [1] == [[1]]
-- True

-- >>> skips [True,False] == [[True,False], [False]]
-- True

-- >>> skips [] == []
-- True

skips' :: [a] -> [[a]]
skips' l = map filtered range
  where
    filtered n = map snd (filter (\(x, _) -> x `mod` n == 0) (zip range l))
    range = [1..(length l)]

-- >>> skips' "ABCD" == ["ABCD", "BD", "C", "D"]
-- True

-- >>> skips' "hello!" == ["hello!", "el!", "l!", "l", "o", "!"]
-- True

-- >>> skips' [True,False] == [[True,False], [False]]
-- True

-- >>> skips' [] == []
-- True


localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:r) = [y | y > x && y > z] ++ localMaxima ([y,z] ++ r)
localMaxima _ = []

-- >>> localMaxima [2,9,5,6,1] == [9,6]
-- True

-- >>> localMaxima [2,3,4,1,5] == [4]
-- True

-- >>> localMaxima [1,2,3,4,5] == []
-- True


histogram :: [Integer] -> String
histogram l = unlines $ 
  map (`line` occurrences) (reverse [1..maximum occurrences])
  ++ ["==========", "0123456789"]
  where
    occurrences = map (`count` l) [0..9]
    count a = length . filter (== a)
    line i = foldl' (\acc each -> acc ++ if each >= i then "*" else " ") ""

-- >>> histogram [1,1,1,5]
-- " *        \n *        \n *   *    \n==========\n0123456789\n"

-- >>> histogram [1,4,5,4,6,6,3,4,2,4,9]
-- "    *     \n    *     \n    * *   \n ******  *\n==========\n0123456789\n"

-- >>> histogram [3,5]
-- "   * *    \n==========\n0123456789\n"
