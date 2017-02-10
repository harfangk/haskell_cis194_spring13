{-
Exercise 2

When I said “we” in the previous sentence I actually meant “you”.
Your task for this exercise is to come up with more efficient implementation.

Specifically, define the infinite list

fibs2 :: [Integer]

so that it has the same elements as fibs1, but computing the first n
elements of fibs2 requires only O(n) addition operations. Be sure to
use standard recursion pattern(s) from the Prelude as appropriate.
-}

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = fibs2_helper 0 1

fibs2_helper :: Integer -> Integer -> [Integer]
fibs2_helper a b = a : fibs2_helper b (a+b)
