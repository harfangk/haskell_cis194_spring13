{-
Exercise 2: Folding with trees

Recall the definition of a binary tree data structure. The height of 
Binary_tree a binary tree is the length of a path from the root to the deepest
node. For example, the height of a tree with a single node is 0; the
height of a tree with three nodes, whose root has two children, is 1;
and so on. A binary tree is balanced if the height of its left and right
subtrees differ by no more than 1, and its left and right subtrees are
also balanced.

You should use the following data structure to represent binary
trees. Note that each node stores an extra Integer representing the
height at that node.

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

For this exercise, write a function

foldTree :: [a] -> Tree a
foldTree = ...
which generates a balanced binary tree from a list of values using
foldr.

For example, one sample output might be the following, also visualized
at right:

foldTree "ABCDEFGHIJ" ==
  Node 3
    (Node 2
      (Node 0 Leaf ’F’ Leaf)
      ’I’
      (Node 1 (Node 0 Leaf ’B’ Leaf) ’C’ Leaf))
    ’J’
    (Node 2
      (Node 1 (Node 0 Leaf ’A’ Leaf) ’G’ Leaf)
      ’H’
      (Node 1 (Node 0 Leaf ’D’ Leaf) ’E’ Leaf))

Your solution might not place the nodes in the same exact order,
but it should result in balanced trees, with each subtree having a
correct computed height.-}

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf 

insert :: a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node n leftTree value rightTree)  
  | leftTreeHeight < rightTreeHeight = Node n (insert x leftTree) value rightTree
  | leftTreeHeight > rightTreeHeight = Node n leftTree value (insert x rightTree)
  | otherwise = Node (rightSubtreeHeight+1) (insert x leftTree) value rightTree
  where leftTreeHeight = heightOfTree leftTree
        rightTreeHeight = heightOfTree rightTree
        rightSubtreeHeight = heightOfTree (insert x rightTree)

heightOfTree :: Tree a -> Integer
heightOfTree Leaf = -1
heightOfTree (Node height _ _ _) = height
