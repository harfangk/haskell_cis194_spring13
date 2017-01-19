{-
Exercise 4

Define the function

validate :: Integer -> Bool

that indicates whether an Integer could be a valid credit card number.
This will use all functions defined in the previous exercises.

Example: validate 4012888888881881 = True
Example: validate 4012888888881882 = False
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

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs)
  | x > 10 = x `mod` 10 + x `div` 10 + sumDigits xs
  | otherwise = x + sumDigits xs
