module Training.CompareVersions (compareVersions) where

-- | Compares version strings
compareVersions :: String -> String -> Ordering
compareVersions v1 v2 = compare (asIntList v1) (asIntList v2)
  where
    asIntList s = map (read :: String -> Int) $ splitBy '.' s

-- | Splits a String by separator
splitBy :: Char -> String -> [String]
splitBy c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : splitBy c s''
    where (w, s'') = break (== c) s'
