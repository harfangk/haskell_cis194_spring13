{-
Exercise 3

Histogram

For this task, write a function

histogram :: [Integer] -> String

which takes as input a list of Integers between 0 and 9 (inclusive),
and outputs a vertical histogram showing how many of each number
were in the input list. You may assume that the input list does not
contain any numbers less than zero or greater than 9 (that is, it does
not matter what your function does if the input does contain such
numbers). Your output must exactly match the output shown in the
examples below.

histogram [1,1,1,5] ==
*
*
* *
==========
0123456789

histogram [1,4,5,4,6,6,3,4,2,4,9] ==
*
*
* *
****** *
==========
0123456789
-}
