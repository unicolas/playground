{-# LANGUAGE InstanceSigs #-}

module Course.Hw10.AParser
  ( char
  , Parser
  , posInt
  , runParser
  , satisfy
  ) where

import Control.Applicative (Alternative (empty, (<|>)), Applicative (liftA2))

import Data.Char (isDigit)
import GHC.Unicode (isUpper)

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------
-- newtype Parser a = Parser (String -> Maybe (a, String))

first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser (fmap (first f) . p)

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser (\s -> Just (x, s))

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) (Parser fab) pa = Parser fb
    where
      fb s = case fab s of
        Nothing -> Nothing
        Just (fab', s') -> runParser (fab' <$> pa) s'

abParser :: Parser (Char, Char)
abParser = liftA2 (,) (char 'a') (char 'b') -- (,) <$> char 'a' <*> char 'b'

{-
>>> runParser abParser "abcdef" == Just (('a','b'),"cdef")
>>> runParser abParser "aebcdf" == Nothing
True
True
-}

abParser_ :: Parser ()
abParser_ = () <$ abParser

{-
>>> runParser abParser_ "abcdef" == Just ((),"cdef")
>>> runParser abParser_ "aebcdf" == Nothing
True
True
-}

intPair :: Parser [Integer]
intPair = discardChar <$> posInt <*> char ' ' <*> posInt
  where discardChar i _ j = [i, j]

{-
>>>runParser intPair "12 34" == Just ([12,34],"")
True
-}

instance Alternative Parser where
  empty :: Parser a
  empty = Parser (const Nothing)

  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) (Parser f1) (Parser f2) = Parser (\s -> f1 s <|> f2 s)

intOrUppercase :: Parser ()
intOrUppercase = () <$ posInt <|> () <$ satisfy isUpper

{-
>>> runParser intOrUppercase "342abcd" == Just ((), "abcd")
>>> runParser intOrUppercase "XYZ" == Just ((), "YZ")
>>> runParser intOrUppercase "foo" == Nothing
True
True
True
-}
