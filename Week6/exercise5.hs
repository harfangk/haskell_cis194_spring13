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
