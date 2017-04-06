{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List

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

battle :: Battlefield -> Rand StdGen Battlefield
battle bf = do
          let attNum = minimum [3, attackers bf - 1]
          let defNum = minimum [2, defenders bf]
          attDie <- sequence (replicate attNum die)
          defDie <- sequence (replicate defNum die)
          let sortedAttDie = map unDV (take 2 (sortBy (flip compare) attDie)) 
          let sortedDefDie = map unDV (take 2 (sortBy (flip compare) defDie)) 
          let results = zipWith (\a b -> a - b) sortedAttDie sortedDefDie
          return (Battlefield (attackers bf - (length $ filter (<= 0) results)) (defenders bf - (length $ filter (> 0) results)))

invade :: Battlefield -> Rand StdGen Battlefield
invade bf 
  | attackers bf < 3 || defenders bf == 0 = return bf
  | otherwise = battle bf >>= invade

successProb :: Battlefield -> Rand StdGen Double
successProb bf = do
               let sampleSize = 1000
               resultArray <- sequence (replicate sampleSize (invade bf))
               let victoryCount = length (filter (\x -> defenders x == 0) resultArray)
               let victoryProb = fromIntegral victoryCount / fromIntegral sampleSize
               return victoryProb
