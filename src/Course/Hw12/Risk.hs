{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Course.Hw12.Risk where

import Control.Monad.Random
  ( MonadRandom(getRandom)
  , Rand
  , Random(randomR, random)
  , replicateM
  , StdGen
  )
import Data.List (sort, foldl')
import Control.Monad (zipWithM)

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving Show

attackerDies :: Battlefield -> Battlefield
attackerDies b =
  Battlefield
    { attackers = attackers b - 1
    , defenders = defenders b
    }

defenderDies :: Battlefield -> Battlefield
defenderDies b =
  Battlefield
    { attackers = attackers b
    , defenders = defenders b - 1
    }

matchup :: Battlefield -> (DieValue, DieValue) -> Battlefield
matchup b (a, d) = if a > d then defenderDies b else attackerDies b

rolls :: Army -> Int -> Rand StdGen [DieValue]
rolls army maxArmy = do
  r <- replicateM (min army maxArmy) die
  pure (reverse . sort $ r)

battle :: Battlefield -> Rand StdGen Battlefield
battle b = do
  attackerRolls <- rolls (attackers b - 1) 3
  defenderRolls <- rolls (defenders b) 2
  matchups <- zipWithM (curry pure) attackerRolls defenderRolls
  pure (foldl' matchup b matchups)

nextBattle :: Battlefield -> Bool
nextBattle (Battlefield a d) = d > 0 && a >= 2

invade :: Battlefield -> Rand StdGen Battlefield
invade b = do
  b' <- battle b
  if nextBattle b' then invade b' else pure b'

successProb :: Battlefield -> Rand StdGen Double
successProb b = do
  simulations <- replicateM 1000 (invade b)
  pure (wins simulations / 1000)
  where
    wins = foldr (\(Battlefield _ d) acc -> if d == 0 then acc + 1 else acc) 0

{-
import Course.Hw12.Risk (Battlefield(..), successProb)
import Control.Monad.Random (evalRandIO)

main :: IO ()
main = do
  r <- evalRandIO . successProb $ Battlefield { attackers = 10, defenders = 12 }
  print r
-}
