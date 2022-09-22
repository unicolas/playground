module Course.HW05.Calc (eval, evalStr, Expr (..)) where

import Course.HW05.ExprT (ExprT (..))
import Course.HW05.Parser (parseExp)

eval :: ExprT -> Integer
eval expr = case expr of
  Lit x -> x
  Add x y -> eval x + eval y
  Mul x y -> eval x * eval y

-- >>> eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20
-- True

evalStr :: String -> Maybe Integer
evalStr s = case parseExp Lit Add Mul s of
  Nothing -> Nothing
  Just expr -> Just (eval expr)

-- >>> evalStr "(2+3)*4" == Just 20
-- True

-- >>> evalStr "2+3*4" == Just 14
-- True

-- >>> evalStr "2+3*" == Nothing
-- True

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit

  mul = Mul

  add = Add

reify :: ExprT -> ExprT
reify = id

-- >>> (mul (add (lit 2) (lit 3)) (lit 4) :: ExprT) == Mul (Add (Lit 2) (Lit 3)) (Lit 4)
-- True

newtype MinMax = MinMax Integer deriving (Eq, Ord, Show)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Integer where
  lit x = x

  mul x y = x * y

  add x y = x + y

instance Expr Bool where
  lit x = x > 0

  mul x y = x && y

  add x y = x || y

instance Expr MinMax where
  lit = MinMax

  mul = min

  add = max

instance Expr Mod7 where
  lit = Mod7

  mul (Mod7 x) (Mod7 y) = Mod7 ((x * y) `mod` 7)

  add (Mod7 x) (Mod7 y) = Mod7 ((x + y) `mod` 7)

-- >>> add (Mod7 5) (Mod7 3)
-- Mod7 1

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

-- >>> testExp
-- Just (-7)

testInteger = testExp :: Maybe Integer

-- >>> testInteger
-- Just (-7)

testBool = testExp :: Maybe Bool

-- >>> testBool
-- Just True

testMM = testExp :: Maybe MinMax

-- >>> testMM
-- Just (MinMax 5)

testSat = testExp :: Maybe Mod7

-- >>> testSat
-- Just (Mod7 0)
