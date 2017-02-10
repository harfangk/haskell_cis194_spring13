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

streamMap :: (a -> b) -> Stream a -> Stream b

streamFromSeed :: (a -> a) -> a -> Stream a
