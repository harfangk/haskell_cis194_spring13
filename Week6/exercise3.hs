{-
Streams

We can be more explicit about infinite lists by defining a type Stream
representing lists that must be infinite. (The usual list type represents
lists that may be infinite but may also have some finite length.)

In particular, streams are like lists but with only a “cons” constructor—
whereas the list type has two constructors, [] (the empty list) and
(:) (cons), there is no such thing as an empty stream. So a stream is
simply defined as an element followed by a stream.

Exercise 3

• Define a data type of polymorphic streams, Stream.
• Write a function to convert a Stream to an infinite list,

streamToList :: Stream a -> [a]

• To test your Stream functions in the succeeding exercises, it will be
useful to have an instance of Show for Streams. However, if you put
deriving Show after your definition of Stream, as one usually does,
the resulting instance will try to print an entire Stream—which,
of course, will never finish. Instead, you should make your own
instance of Show for Stream,

instance Show a => Show (Stream a) where
  show ...

which works by showing only some prefix of a stream (say, the
first 20 elements).
-}

fib :: Integer -> Integer

fibs1 :: [Integer]

fibs2 :: [Integer]

streamToList :: Stream a -> [a]
