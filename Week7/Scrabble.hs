{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scrabble where 

import Data.Char
import Data.Monoid

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

instance Monoid Score where
  mempty = Score 0
  mappend = (+)

getScore :: Score -> Int
getScore (Score i) = i

score :: Char -> Score
score c 
  | elem char "aeilnorstu" = Score 1
  | elem char "dg" = Score 2
  | elem char "bcmp" = Score 3
  | elem char "fhvwy" = Score 4
  | elem char "k" = Score 5
  | elem char "jx" = Score 8
  | elem char "qz'" = Score 10
  | otherwise = Score 0
  where char = toLower c

scoreString :: String -> Score
scoreString = foldl (\x c -> x + score c) (Score 0)
