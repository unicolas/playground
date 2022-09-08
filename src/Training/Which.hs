module Training.Which (areIn) where

import Data.List (group, isInfixOf, sort) 

-- | Which are in?
areIn :: [String] -> [String] -> [String]
areIn a1 a2 = filter (\e -> any (isInfixOf e) a2) sortedList
  where
    sortedList = map head $ group $ sort a1

