module Course.Hw03.Golf (skips) where

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
