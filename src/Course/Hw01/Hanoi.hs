module Course.Hw01.Hanoi (hanoi) where

type Peg = String

type Move = (Peg, Peg)

-- | Returns a list of moves to be performed to move the stack of discs from the
-- first peg to the second, given the number of discs and names for the three pegs.
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n <= 0 = []
  | n == 1 = [(a, b)]
  | otherwise = hanoi (n - 1) a c b ++ [(a, b)] ++ hanoi (n - 1) c b a
