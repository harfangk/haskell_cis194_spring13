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
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = fibs2_helper 0 1

fibs2_helper :: Integer -> Integer -> [Integer]
fibs2_helper a b = a : fibs2_helper b (a+b)

data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = ruler_helper 0

ruler_helper :: Integer -> Stream Integer
ruler_helper x = interleaveStreams (streamRepeat x) (ruler_helper (x+1))

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) ys = Cons x (interleaveStreams ys xs)

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger n = Cons n (streamRepeat 0)
  negate x = streamMap negate x
  (+) (Cons x xs) (Cons y ys) = Cons (x + y) (xs + ys)
  (*) (Cons x xs) (Cons y ys) = Cons (x * y) (streamMap (*x) ys + xs * (Cons y ys))
  
instance Fractional (Stream Integer) where
  (/) (Cons x xs) (Cons y ys) = q 
    where q = Cons (div x y) (streamMap (`div` y) (xs - (q * ys)))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x ^ (2 :: Integer))

data Matrix = Matrix Integer Integer Integer Integer

instance Num Matrix where
  (*) (Matrix a b c d) (Matrix e f g h) = Matrix (a*e + b*g) (a*f + b*h) (c*e + d*g) (c*f + d*h)

fibs4 :: Integer -> Integer
fibs4 0 = 0
fibs4 n = getSecond((Matrix 1 1 1 0) ^ n)

getSecond :: Matrix -> Integer
getSecond (Matrix _ x _ _) = x
