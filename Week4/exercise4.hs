{-
Exercise 4: Finding primes

Read about the Sieve of Sundaram. Implement the algorithm using
function composition. Given an integer n, your function should
generate all the odd prime numbers up to 2n + 2.

sieveSundaram :: Integer -> [Integer]
sieveSundaram = ...

To give you some help, below is a function to compute the Cartesian
product of two lists. This is similar to zip, but it produces all
possible pairs instead of matching up the list elements. For example,

cartProd [1,2] [’a’,’b’] == [(1,’a’),(1,’b’),(2,’a’),(2,’b’)]

It’s written using a list comprehension, which we haven’t talked about
in class (but feel free to research them).

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]-}

import Data.List

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2*x + 1) ([1..n] \\ [i+j+2*i*j | i <- [1..n], j <- [1..n], i+j+2*i*j < n])
