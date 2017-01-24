{-
Exercise 3 Histogram

For this task, write a function

histogram :: [Integer] -> String

which takes as input a list of Integers between 0 and 9 (inclusive),
and outputs a vertical histogram showing how many of each number
were in the input list. You may assume that the input list does not
contain any numbers less than zero or greater than 9 (that is, it does
not matter what your function does if the input does contain such
numbers). Your output must exactly match the output shown in the
examples below.

histogram [1,1,1,5] ==
 *
 *
 *   *
==========
0123456789

histogram [1,4,5,4,6,6,3,4,2,4,9] ==
    *
    *
    * *
 ******  *
==========
0123456789
-}

import Data.List

frequency :: [Integer] -> [(Int, Integer)]
frequency xs = zip (map length $ group $ sort xs) (nub xs)

maxFrequency :: [Integer] -> Int
maxFrequency [] = 0
maxFrequency xs = 
  case (maximum . frequency) xs of
    (frequency, element) -> frequency

findEmptyInteger :: Integer -> [Integer] -> [Integer]
findEmptyInteger maxRange xs = [0..maxRange] \\ xs

buildMissingTuple :: [Integer] -> [(Int, Integer)]
buildMissingTuple = map (\ x -> (0, x)) 

toStringList :: [(Int, Integer)] -> Int -> [String]
toStringList [] _ = []
toStringList ((count, element):ys) maxFreq = (show element ++ "=" ++ concat (replicate count "*") ++ concat (replicate (maxFreq  - count) " ")) : toStringList ys maxFreq

histogram :: [Integer] -> String
histogram list = intercalate "\n" (reverse (transpose (sort (toStringList (frequency list ++ buildMissingTuple (findEmptyInteger 9 list)) (maxFrequency list)))))
