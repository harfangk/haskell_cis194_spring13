{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-
Exercise 7 (Optional)

• Create a type Matrix which represents 2 × 2 matrices of Integers.

• Make an instance of the Num type class for Matrix. In fact, you only
have to implement the (*) method, since that is the only one we
will use. (If you want to play around with matrix operations a bit
more, you can implement fromInteger, negate, and (+) as well.)

• We now get fast (logarithmic time) matrix exponentiation for free,
since (^) is implemented using a binary exponentiation algorithm
in terms of (*). Write a function

fib4 :: Integer -> Integer

which computes the nth Fibonacci number by raising F to the nth
power and projecting out Fn (you will also need a special case
for zero). Try computing the one millionth or even ten millionth
Fibonacci number. 
-}

fib :: Integer -> Integer

fibs1 :: [Integer]

fibs2 :: [Integer]

streamToList :: Stream a -> [a]

streamRepeat :: a -> Stream a

streamMap :: (a -> b) -> Stream a -> Stream b

streamFromSeed :: (a -> a) -> a -> Stream a

nats :: Stream Integer

ruler :: Stream Integer

fib3 :: Integer -> Integer

fib4 :: Integer -> Integer
