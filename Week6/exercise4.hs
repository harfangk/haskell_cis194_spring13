{-
Exercise 4

Let’s create some simple tools for working with Streams.

• Write a function

streamRepeat :: a -> Stream a

which generates a stream containing infinitely many copies of the
given element.

• Write a function

streamMap :: (a -> b) -> Stream a -> Stream b

which applies a function to every element of a Stream.

• Write a function

streamFromSeed :: (a -> a) -> a -> Stream a

which generates a Stream from a “seed” of type a, which is the
first element of the stream, and an “unfolding rule” of type a -> a
which specifies how to transform the seed into a new seed, to be
used for generating the rest of the stream.

Hint: you may find your streamToList function useful.
-}

fib :: Integer -> Integer

fibs1 :: [Integer]

fibs2 :: [Integer]

streamToList :: Stream a -> [a]

streamRepeat :: a -> Stream a

streamMap :: (a -> b) -> Stream a -> Stream b

streamFromSeed :: (a -> a) -> a -> Stream a
