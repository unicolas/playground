{-# LANGUAGE FlexibleInstances #-}

module Course.Hw07.JoinList where

import Data.Monoid ()
import Course.Hw07.Sized (Sized, getSize, size, Size(..))
import Course.Hw07.Scrabble (Score(..), scoreString)
import Course.Hw07.Buffer
  ( Buffer
  , toString
  , fromString
  , line
  , replaceLine
  , numLines
  , value
  )
import Data.List (foldl')

data JoinList m a
  = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) lst1 lst2 = case (lst1, lst2) of
  (Empty, _) -> lst2
  (_, Empty) -> lst1
  _ -> Append (tag lst1 <> tag lst2) lst1 lst2

tag :: Monoid m => JoinList m a -> m
tag lst = case lst of
  Empty -> mempty
  Single m _ -> m
  Append m _ _ -> m

{-
>>> import Data.Semigroup (Product(..))
>>> Single (Product 2) 'e' +++ Single (Product 3) 'a'
Append (Product {getProduct = 6}) (Single (Product {getProduct = 2}) 'e') (Single (Product {getProduct = 3}) 'a')
-}

sizeOf :: (Sized a1, Monoid a1) => JoinList a1 a2 -> Int
sizeOf l = getSize $ size $ tag l

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ n jl = case jl of
  Empty -> Nothing
  _ | n < 0 -> Nothing
  _ | n > sizeOf jl -> Nothing
  Single _ a -> Just a
  Append _ l r
    | n < sizeOf l -> indexJ n l
    | otherwise -> indexJ (n `div` 2) r

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n jl = case jl of
  Empty -> jl
  _ | n < 0 -> jl
  Single _ _ -> jl
  Append _ l r
    | n >= sizeOf jl -> Empty
    | n < sizeOf l -> dropJ n l +++ r
    | otherwise -> dropJ (n `div` 2) r

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n jl = case jl of
  Empty -> jl
  _ | n <= 0 -> Empty
  _ | n >= sizeOf jl -> jl
  Single _ _ -> jl
  Append _ l r
    | n <= sizeOf l -> takeJ n l
    | otherwise -> l +++ takeJ (n `div` 2) r


{- for testing -}

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

{-
>>> import Course.Hw07.Sized (Size (Size))
>>> jl = Append (Size 4) (Append (Size 3) (Single (Size 1) 'y') (Append (Size 2) (Single (Size 1) 'e') (Single (Size 1) 'a'))) (Single (Size 1) 'h')
>>> (indexJ 1 jl) == (jlToList jl !!? 1)
True

>>> jl = Append (Size 4) (Append (Size 3) (Single (Size 1) 'y') (Append (Size 2) (Single (Size 1) 'e') (Single (Size 1) 'a'))) (Single (Size 1) 'h')
>>> [indexJ (-1) jl, indexJ 0 jl, indexJ 1 jl, indexJ 2 jl, indexJ 3 jl, indexJ 4 jl] == [Nothing, Just 'y', Just 'e', Just 'a', Just 'h', Nothing]
True

>>> jl = Append (Size 4) (Append (Size 3) (Single (Size 1) 'y') (Append (Size 2) (Single (Size 1) 'e') (Single (Size 1) 'a'))) (Single (Size 1) 'h')
>>> jlToList (dropJ 4 jl) == drop 4 (jlToList jl)
True

>>> jl = Append (Size 4) (Append (Size 3) (Single (Size 1) 'y') (Append (Size 2) (Single (Size 1) 'e') (Single (Size 1) 'a'))) (Single (Size 1) 'h')
>>> jlToList (takeJ 2 jl) == take 2 (jlToList jl)
True
-}

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

{-
>>> scoreLine "yay " +++ scoreLine "haskell!"
Append (Score 23) (Single (Score 9) "yay ") (Single (Score 14) "haskell!")
-}

instance Buffer (JoinList (Score, Size) String) where
  toString jl = case jl of
    Empty -> ""
    Single _ ln -> ln
    Append _ l r -> toString l ++ "\n" ++ toString r

  fromString s -- Unbalanced
    = foldl' (\jl ln -> jl +++ Single (scoreString ln, Size 1) ln) Empty
    $ lines s

  line = indexJ

  replaceLine n s jl
    | n <= 0 = jl
    | n > sizeOf jl = jl
    | otherwise = takeJ (n - 1) jl +++ fromString s +++ dropJ n jl
  
  numLines = sizeOf

  value l = case fst $ tag l of Score s -> s

{-
>>> fromString "yay \n haskell!\nAnother line." :: JoinList (Score, Size) String
Append (Score 37,Size 3) (Append (Score 23,Size 2) (Single (Score 9,Size 1) "yay ") (Single (Score 14,Size 1) " haskell!")) (Single (Score 14,Size 1) "Another line.")

>>> l = fromString "yay \n haskell!\nAnother line." :: JoinList (Score, Size) String
>>> toString l == "yay \n haskell!\nAnother line."
True

>>> l = fromString "yay \n haskell!\nAnother line." :: JoinList (Score, Size) String
>>> line 3 l
Just "Another line."

>>> l = fromString "yay \n haskell!\nAnother line." :: JoinList (Score, Size) String
>>> numLines l
3

>>> l = fromString "yay \n haskell!\nAnother line." :: JoinList (Score, Size) String
>>> value l
37

>>> l = fromString "yay \n haskell!\nAnother line." :: JoinList (Score, Size) String
>>> toString (replaceLine 0 "oh no!" l) == "yay \n haskell!\nAnother line."
>>> toString (replaceLine 1 "oh no!" l) == "oh no!\n haskell!\nAnother line."
>>> toString (replaceLine 2 "oh no!" l) == "yay \noh no!\nAnother line."
>>> toString (replaceLine 3 "oh no!" l) == "yay \n haskell!\noh no!"
>>> toString (replaceLine 4 "oh no!" l) == "yay \n haskell!\nAnother line."
True
True
True
True
True
-}
