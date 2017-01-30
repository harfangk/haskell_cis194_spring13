{-
Exercise 3: More folds!

1. Implement a function

xor :: [Bool] -> Bool

which returns True if and only if there are an odd number of True
values contained in the input list. It does not matter how many
False values the input list contains. For example,

xor [False, True, False] == True
xor [False, True, False, False, True] == False

Your solution must be implemented using a fold.

2. Implement map as a fold. That is, complete the definition
map’ :: (a -> b) -> [a] -> [b]
map’ f = foldr ...
in such a way that map’ behaves identically to the standard map
function.

3. (Optional) Implement foldl using foldr. That is, complete the
definition

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr ...
in such a way that myFoldl behaves identically to the standard
foldl function.

Hint: Study how the application of foldr and foldl work out:
foldr f z [x1, x2, ..., xn] == x1 ‘f‘ (x2 ‘f‘ ... (xn ‘f‘ z)...)
foldl f z [x1, x2, ..., xn] == (...((z ‘f‘ x1) ‘f‘ x2) ‘f‘...) ‘f‘ xn-}

xor :: [Bool] -> Bool
xor = foldl (\acc _ -> not acc) False . filter (==True)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

myfoldl :: (a -> b -> a) -> a -> [b] -> a
myfoldl f base xs = foldr (flip f) base xs
