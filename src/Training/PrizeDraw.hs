module Training.PrizeDraw (rank) where

import Data.Char (toLower)
import Data.List (elemIndex, foldl1', sortBy)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)

-- | Rank
rank :: [Char] -> [Int] -> Int -> [Char]
rank st we n
  | null st = "No participants"
  | length names < n = "Not enough participants"
  | otherwise = snd $ last $ take n (namesAndNumbers names (winningNumbers names we))
  where names = splitBy ',' st

-- | Gives the value for a letter
letterValue :: Char -> Int
letterValue letter = fromMaybe (-1) (elemIndex (toLower letter) ['a'..'z']) + 1

-- | Gives the name value (value sum + length)
nameValue :: [Char] -> Int
nameValue name = foldl1' (+) (map letterValue name) + length name

-- | Gives the winning numbers
winningNumbers :: [String] -> [Int] -> [Int]
winningNumbers names weights = zipWith (*) weights (map nameValue names)

-- | Gives names and numbers sorted asc by numbers and desc by names
namesAndNumbers :: [String] -> [Int] -> [(Int, String)]
namesAndNumbers names numbers = sortBy descAsc $ zip numbers names
  where descAsc = flip (comparing fst) <> comparing snd

-- | Splits a String by separator
splitBy :: Char -> String -> [String]
splitBy c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : splitBy c s''
    where (w, s'') = break (== c) s'
