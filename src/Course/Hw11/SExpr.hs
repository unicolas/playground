module Course.Hw11.SExpr where

import Course.Hw10.AParser (Parser, runParser, satisfy, char, posInt)
import Control.Applicative
import Data.Char (isUpper, isSpace, isAlpha, isAlphaNum)

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore parser = oneOrMore parser <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore parser = (:) <$> parser <*> zeroOrMore parser

{-
>>> runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH" == Just ("ABC","dEfgH")
>>> runParser (oneOrMore (satisfy isUpper)) "ABCdEfgH" == Just ("ABC","dEfgH")
>>> runParser (zeroOrMore (satisfy isUpper)) "abcdeFGh" == Just ("","abcdeFGh")
>>> runParser (oneOrMore (satisfy isUpper)) "abcdeFGh" == Nothing
True
True
True
True
-}

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
ident = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)
-- ident = (++) <$> oneOrMore (satisfy isAlpha) <*> zeroOrMore (satisfy isAlphaNum)

{-
>>> runParser ident "foobar baz" == Just ("foobar"," baz")
>>> runParser ident "foo33fA" == Just ("foo33fA","")
>>> runParser ident "2bad" == Nothing
>>> runParser ident "" == Nothing
True
True
True
True
-}

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

parseSExpr :: Parser SExpr
parseSExpr = spaces *> (A <$> atomParser <|> Comb <$> combParser) <* spaces
  where
    atomParser = N <$> posInt <|> I <$> ident
    combParser = char '(' *> oneOrMore parseSExpr <* char ')'

{-
>>> runParser parseSExpr "5"
>>> runParser parseSExpr "foo3"
>>> runParser parseSExpr "(bar (foo) 3 5 874)"
>>> runParser parseSExpr "(((lambda x (lambda y (plus x y))) 3) 5)"
>>> runParser parseSExpr "( lots of ( spaces in ) this ( one ) )"
Just (A (N 5),"")
Just (A (I "foo3"),"")
Just (Comb [A (I "bar"),Comb [A (I "foo")],A (N 3),A (N 5),A (N 874)],"")
Just (Comb [Comb [Comb [A (I "lambda"),A (I "x"),Comb [A (I "lambda"),A (I "y"),Comb [A (I "plus"),A (I "x"),A (I "y")]]],A (N 3)],A (N 5)],"")
Just (Comb [A (I "lots"),A (I "of"),Comb [A (I "spaces"),A (I "in")],A (I "this"),Comb [A (I "one")]],"")
-}
