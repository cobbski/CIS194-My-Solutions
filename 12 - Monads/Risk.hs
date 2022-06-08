-- CIS 194 - Homework 12

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <$>" #-}

module Risk where

import Control.Monad.Random
import Data.List (sort)

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
  deriving (Show)


-- Exercise 2

{- Given the number of rolls for each player, make the dicerolls and then pair them in descending order -}
unitRolls :: Int -> Int -> Rand StdGen [(DieValue, DieValue)]
unitRolls a d = (zip . descending a <$> getRandoms) <*> (descending d <$> getRandoms)
              where descending x = reverse . sort . take x

{- Given roll outcomes, get number of units the attacking player lost,
   and number defending player lost -}
unitLosses :: [(DieValue, DieValue)] -> Rand StdGen (Int, Int)
unitLosses pairs =
  return (length $ filter (uncurry (<=)) pairs,
          length $ filter (uncurry (>)) pairs)

battle :: Battlefield -> Rand StdGen Battlefield
battle bf = 
  unitRolls (min aUnits 3) (min dUnits 2) >>=
  unitLosses >>= \(aLosses, dLosses) ->
  return Battlefield { attackers = aUnits - aLosses, defenders = dUnits - dLosses }
    where aUnits = attackers bf
          dUnits = defenders bf


-- Exercise 3
invade :: Battlefield -> Rand StdGen Battlefield
invade bf =
  battle bf >>= \bf' ->
    if attackers bf' > 0 && defenders bf' > 0 then invade bf' else return bf'


-- Exercise 4
{- Infinite list of simulations of invade on a battlefield -}
simulateInvade :: Battlefield -> Rand StdGen [Battlefield]
simulateInvade bf = invade bf >>= \bf' -> (bf' :) <$> simulateInvade bf

successProb :: Battlefield -> Rand StdGen Double
successProb bf =
  take 1000 <$> simulateInvade bf >>= \bfs ->
  return $ (fromIntegral . length $ filter (\i -> defenders i == 0) bfs) / 1000