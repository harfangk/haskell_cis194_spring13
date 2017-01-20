{-
Exercise 6 (Optional)

What if there are four pegs instead of three?
That is, the goal is still to move a stack of discs from the first peg to
the last peg, without ever placing a larger disc on top of a smaller
one, but now there are two extra pegs that can be used as “temporary”
storage instead of only one. Write a function similar to hanoi
which solves this problem in as few moves as possible.

It should be possible to do it in far fewer moves than with three
pegs. For example, with three pegs it takes 215 − 1 = 32767 moves
to transfer 15 discs. With four pegs it can be done in 129 moves.

(See Exercise 1.17 in Graham, Knuth, and Patashnik,
Concrete Mathematics, second ed., Addison-Wesley, 1994.) 
 -}

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi n source target tempStore1 tempStore2
  | n == 0    = []
  | otherwise = hanoi (findTemporaryHeight n) source tempStore1 target tempStore2
                ++ hanoi3 (n-(findTemporaryHeight n)) source target tempStore2 ++
                hanoi (findTemporaryHeight n) tempStore1 target source tempStore2

findTemporaryHeight :: Integer -> Integer
findTemporaryHeight n = n + 1 - round(sqrt (fromIntegral((2*n) + 1)))

hanoi3 :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi3 n source target tempStore
  | n == 0    = []
  | otherwise = hanoi3 (n-1) source tempStore target
                ++ [(source, target)] ++
                hanoi3 (n-1) tempStore target source
