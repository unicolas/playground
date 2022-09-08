module Training.BreakingChocolate (breakChocolate) where

breakChocolate :: Int -> Int -> Int
breakChocolate _ 0 = 0
breakChocolate 0 _ = 0
breakChocolate n m = n * m - 1
