{-
Fibonacci numbers

The Fibonacci numbers Fn are defined as the sequence of integers,
beginning with 0 and 1, where every integer in the sequence is the
sum of the previous two. That is,

F0 = 0
F1 = 1
Fn = Fn−1 + Fn−2 (n ≥ 2)

For example, the first fifteen Fibonacci numbers are
0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, . . .
It’s quite likely that you’ve heard of the Fibonacci numbers before.
The reason they’re so famous probably has something to do with the
simplicity of their definition combined with the astounding variety of
ways that they show up in various areas of mathematics as well as art
and nature.

Exercise 1

Translate the above definition of Fibonacci numbers directly into a
recursive function definition of type

fib :: Integer -> Integer

so that fib n computes the nth Fibonacci number Fn.
Now use fib to define the infinite list of all Fibonacci numbers,

fibs1 :: [Integer]

(Hint: You can write the list of all positive integers as [0..].)

Try evaluating fibs1 at the ghci prompt. You will probably get
bored watching it after the first 30 or so Fibonacci numbers, because
fib is ridiculously slow. Although it is a good way to define the Fibonacci
numbers, it is not a very good way to compute them—in order
to compute Fn it essentially ends up adding 1 to itself Fn times! For
example, shown at right is the tree of recursive calls made by evaluating
fib 5.

As you can see, it does a lot of repeated work. In the end, fib
has running time O(Fn), which (it turns out) is equivalent to O(ϕ^n),
where ϕ = (1+√5)/2 is the “golden ratio”. That’s right, the running time
is exponential in n. What’s more, all this work is also repeated from
each element of the list fibs1 to the next. Surely we can do better.
-}

fib :: Integer -> Integer

fibs1 :: [Integer]
