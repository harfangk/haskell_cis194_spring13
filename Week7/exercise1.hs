{-
Exercise 1

We first consider how to write some simple operations
on these JoinLists. Perhaps the most important operation we will
consider is how to append two JoinLists. Previously, we said that
the point of JoinLists is to represent append operations as data, but
what about the annotations? Write an append function for JoinLists
that yields a new JoinList whose monoidal annotation is derived
from those of the two arguments.

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a

You may find it helpful to implement a helper function

tag :: Monoid m => JoinList m a -> m

which gets the annotation at the root of a JoinList.
-}
