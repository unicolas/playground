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

indexJ' :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ' s lst = case (s, lst) of
  (_, Empty) -> Nothing
  (x, _) | x < 0 -> Nothing
  (x, jl) | x > sizeOf jl -> Nothing
  (_, Single _ a) -> Just a
  (x, Append _ l _) | x < sizeOf l -> indexJ x l
  (x, Append _ _ r) -> indexJ (x `div` 2) r
  where
    sizeOf jl = getSize $ size $ tag jl

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
-}

