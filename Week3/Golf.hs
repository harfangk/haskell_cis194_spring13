import Data.List
import Control.Monad

frequency :: [Integer] -> [(Int, Integer)]
frequency = ap (zip . map length . group . sort) nub

maxFrequency :: [Integer] -> Int
maxFrequency [] = 0
maxFrequency xs = 
  case (maximum . frequency) xs of
    (frequency, element) -> frequency

findEmptyInteger :: Integer -> [Integer] -> [Integer]
findEmptyInteger = (\\) . enumFromTo 0

buildMissingTuple :: [Integer] -> [(Int, Integer)]
buildMissingTuple = map (\ x -> (0, x)) 

toStringList :: [(Int, Integer)] -> Int -> [String]
toStringList [] _ = []
toStringList ((count, element):ys) maxFreq = (show element ++ "=" ++ concat (replicate count "*") ++ concat (replicate (maxFreq - count) " ")) : toStringList ys maxFreq

histogram :: [Integer] -> String
histogram = intercalate "\n" . reverse . transpose . sort . ap (toStringList . liftM2 (++) frequency (buildMissingTuple . findEmptyInteger 9)) maxFrequency
