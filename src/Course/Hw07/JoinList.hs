module Course.Hw07.JoinList where

import Data.Monoid ()
import Course.Hw07.Sized (Sized, getSize, size)

data JoinList m a
  = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) lst1 lst2 = Append (tag lst1 <> tag lst2) lst1 lst2

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

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ x j | x < 0 || x > getSize (size (tag j)) = Nothing
indexJ _ (Single _ a) = Just a
indexJ x (Append _ l r) =
  if x < getSize (size (tag l))
  then indexJ x l
  else indexJ (x `div` 2) r

sizeOf :: (Sized a1, Monoid a1) => JoinList a1 a2 -> Int
sizeOf l = getSize $ size $ tag l

indexJ' :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ' n jl = case jl of
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
  _ | n > sizeOf jl -> Empty
  Single _ _ -> jl
  Append _ l r
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
>>> jlToList (dropJ 3 jl) == drop 3 (jlToList jl)
True

>>> jl = Append (Size 4) (Append (Size 3) (Single (Size 1) 'y') (Append (Size 2) (Single (Size 1) 'e') (Single (Size 1) 'a'))) (Single (Size 1) 'h')
>>> jlToList (takeJ 2 jl) == take 2 (jlToList jl)
True
-}

