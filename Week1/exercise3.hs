{-
Exercise 3

The output of doubleEveryOther has a mix of one-digit and two-digit numbers. Define the function

sumDigits :: [Integer] -> Integer

to calculate the sum of all digits.

Example: sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22
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
