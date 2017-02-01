{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-
Exercise 6 (Optional)

• First, define

x :: Stream Integer

by noting that x = 0 + 1x + 0x 2 + 0x 3 + . . . .

• Define an instance of the Num type class for Stream Integer.
Note that you will have to add {-# LANGUAGE FlexibleInstances #-}
to the top of your .hs file in order for this instance to be allowed.

Here’s what should go in your Num instance:

– You should implement the fromInteger function. Note that
n = n + 0x + 0x 2 + 0x 3 + . . . .

– You should implement negate: to negate a generating function,
negate all its coefficients.

– You should implement (+), which works like you would expect:
(a0 + a1x + a2x 2 + . . .) + (b0 + b1x + b2x 2 + . . .) =
  (a0 + b0) + (a1 + b1)x + (a2 + b2)x 2 + . . .

– Multiplication is a bit trickier. Suppose A = a0 + xA0 and
B = b0 + xB0 are two generating functions we wish to multiply.
We reason as follows:
AB  = (a0 + xA')B
    = a0B + xA'B
    = a0(b0 + xB') + xA'B
    = a0b0 + x(a0B' + A'B)

That is, the first element of the product AB is the product of
the first elements, a0b0; the remainder of the coefficient stream
(the part after the x) is formed by multiplying every element in
B' (that is, the tail of B) by a0, and to this adding the result of
multiplying A' (the tail of A) by B.

Note that there are a few methods of the Num class I have not
told you to implement, such as abs and signum. ghc will complain
that you haven’t defined them, but don’t worry about it. We won’t
need those methods. (To turn off these warnings you can add
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
to the top of your file.)

If you have implemented the above correctly, you should be able
to evaluate things at the ghci prompt such as
*Main> x^4
*Main> (1 + x)^5
*Main> (x^2 + x + 3) * (x - 5)

• The penultimate step is to implement an instance of the Fractional
class for Stream Integer. Here the important method to define is
division, (/). I won’t bother deriving it (though it isn’t hard), but
it turns out that if A = a0 + xA' and B = b0 + xB', then A/B = Q,
where Q is defined as
Q = (a0/b0) + x((1/b0)(A 0 − QB0)).
That is, the first element of the result is a0/b0; the remainder is
formed by computing A' − QB' and dividing each of its elements
by b0.

Of course, in general, this operation might not result in a stream
of Integers. However, we will only be using this instance in cases
where it does, so just use the div operation where appropriate.

• Consider representing the Fibonacci numbers using a generating
function,
F(x) = F0 + F1x + F2x^2 + F3x^3 + . . .

Notice that x + xF(x) + x 2F(x) = F(x):
    x
  F0x + F1x^2 + F2x^3 + F3x^4 + . . .
        F0x^2 + F1x^3 + F2x^4 + . . .
========================================
0 + x + F2x^2 + F3x^3 + F4x^4 + . . .

Thus x = F(x) − xF(x) − x^2F(x), and solving for F(x) we find
that
F(x) = x / (1 − x − x^2) .
Translate this into an (amazing, totally sweet) definition
fibs3 :: Stream Integer
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
