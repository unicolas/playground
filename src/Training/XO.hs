module Training.XO (xo) where

-- | Exes and Ohs
xo :: String -> Bool
xo str = length (filterElem "xX" str) == length (filterElem "oO" str)
  where filterElem e = filter (`elem` e)
