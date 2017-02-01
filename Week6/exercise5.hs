{-
Exercise 5

Now that we have some tools for working with streams, let’s create a few:

• Define the stream

nats :: Stream Integer

which contains the infinite list of natural numbers 0, 1, 2, . . .

• Define the stream

ruler :: Stream Integer

which corresponds to the ruler function
0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, . . .
where the nth element in the stream (assuming the first element
corresponds to n = 1) is the largest power of 2 which evenly
divides n.

Hint: define a function interleaveStreams which alternates the elements
from two streams. Can you use this function to implement ruler in
a clever way that does not have to do any divisibility testing? 
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
