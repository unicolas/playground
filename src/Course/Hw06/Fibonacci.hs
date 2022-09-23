module Course.Hw06.Fibonacci () where

fib :: Integer -> Integer
fib n = case n of
  0 -> 0
  1 -> 1
  _ -> fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = fibSum 0 1
  where fibSum x y = x : fibSum y (x + y)


{- Streams -}

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs)  = x : streamToList xs

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = Cons seed (streamFromSeed f (f seed))

nats :: Stream Integer
nats = streamFromSeed (1 +) 0

ruler :: Stream Integer
ruler = generator 0
  where generator x = interleaveStreams (streamRepeat x) (generator (x + 1))

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) ys = Cons x (interleaveStreams ys xs)

-- 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 5
-- 1, 2, 1, 3, 1, 2, 1, 4, 1, 2, 1, 3, 1, 2, 1, 5 -> 0
-- 2, 3, 2, 4, 2, 3, 2, 5 -> 1
-- 3, 4, 3, 5 -> 2
-- 4, 5 -> 3

-- >>> ruler
-- [0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4,0,1,0,2]
