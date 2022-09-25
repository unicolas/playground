{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Course.Hw07.Scrabble (Score(..), score, scoreString) where

import Data.Char (toUpper)

newtype Score = Score Int deriving (Eq, Ord, Show, Num)

instance Semigroup Score where
  (<>) = (+)

instance Monoid Score where
  mempty = Score 0

score :: Char -> Score
score c = case toUpper c of
  'A' -> 1; 'B' -> 3; 'C' -> 3; 'D' -> 2; 'E' -> 1; 'F' -> 4
  'G' -> 2; 'H' -> 4; 'I' -> 1; 'J' -> 8; 'K' -> 5; 'L' -> 1
  'M' -> 3; 'N' -> 1; 'O' -> 1; 'P' -> 3; 'Q' -> 10; 'R' -> 1
  'S' -> 1; 'T' -> 1; 'U' -> 1; 'V' -> 4; 'W' -> 4; 'X' -> 8
  'Y' -> 4; 'Z' -> 10; _ -> 0

scoreString :: String -> Score
scoreString = foldr ((<>) . score) mempty
