module Training.Section (section) where

import Data.List (elemIndex, find)

-- | Which section did you scroll to?
section :: Int -> [Int] -> Maybe Int
section scrollY sizes = case sumSize of
  Just a -> elemIndex a sumSizes 
  Nothing -> Nothing
  where 
    sumSizes = scanl1 (+) sizes
    sumSize = find (> scrollY) sumSizes
