toDigits :: Integer -> [Integer]
toDigits input
  | input <= 0 = []
  | otherwise = reverse(toDigitsRev input)


toDigitsRev :: Integer -> [Integer]
toDigitsRev input
  | input <= 0 = []
  | otherwise = input `mod` 10 : toDigitsRev(input `div` 10)
