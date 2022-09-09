module Training.StringsMix (mix) where

import Data.List (sortBy, intercalate)

mix :: [Char] -> [Char] -> [Char]
mix s1 s2 = 
  intercalate "/" $
  map (\(a, b, c) -> c ++ times a [b]) $
  occurrences s1 s2
  where 
    times n str = concat (replicate n str)

-- | Returns filtered and sorted occcurrence list.
occurrences :: [Char] -> [Char] -> [(Int, Char, [Char])]
occurrences s1 s2 = 
  sortBy cmp $
  filter (\(x, _, _) -> x > 1) $
  map (\c -> countBoth c s1 s2) ['a'..'z']
  where
    cmp (a, b, c) (x, y, z) = case compare a x of
      GT -> LT
      LT -> GT
      EQ -> if (c ++ [b]) < (z ++ [y]) then LT else GT

-- | Returns max occurrence (max occurrence, char, max prefix) of both lists.
countBoth :: Char -> [Char] -> [Char] -> (Int, Char, [Char])
countBoth chr lst1 lst2 = maxFor (countIn lst1) (countIn lst2)
  where
    countIn lst = if null lst then 0 else length $ filter (== chr) lst
    maxFor x y = case compare x y of
      GT -> (x, chr, "1:") 
      LT -> (y, chr, "2:")
      EQ -> (x, chr, "=:")
