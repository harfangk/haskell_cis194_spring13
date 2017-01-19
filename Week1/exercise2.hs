{-
Exercise 2

Once we have the digits in the proper order, we need to double every other one. Define a function

doubleEveryOther :: [Integer] -> [Integer]

Remember that doubleEveryOther should double every other number beginning from the right,
that is, the second-to-last, fourth-to-last, ... numbers are doubled.

Example: doubleEveryOther [8,7,6,5] == [16,7,12,5]
Example: doubleEveryOther [1,2,3] == [1,4,3]
-}

toDigits :: Integer -> [Integer]
toDigits input
  | input <= 0 = []
  | otherwise = reverse(toDigitsRev input)

toDigitsRev :: Integer -> [Integer]
toDigitsRev input
  | input <= 0 = []
  | otherwise = input `mod` 10 : toDigitsRev(input `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther [x, y] = [2*x, y]
doubleEveryOther (x:y:ys)
  | length ys `mod` 2 == 1 = x : y*2 : doubleEveryOther ys
  | otherwise = 2*x : y : doubleEveryOther ys
