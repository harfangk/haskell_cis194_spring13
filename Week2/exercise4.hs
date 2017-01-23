{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

{-
Exercise 4

Finally, define the function

inOrder :: MessageTree -> [LogMessage]

which takes a sorted MessageTree and produces a list of all the
LogMessages it contains, sorted by timestamp from smallest to biggest.
(This is known as an in-order traversal of the MessageTree.)

With these functions, we can now remove Unknown messages and
sort the well-formed messages using an expression such as:

inOrder (build tree)

[Note: there are much better ways to sort a list; this is just an exercise
to get you working with recursive data structures!]
-}

parseMessage :: String -> LogMessage
parseMessage string =
  case words string of
    ("E":severity:timeStamp:xs) -> LogMessage (Error (read severity)) (read timeStamp) (unwords xs)
    ("I":timeStamp:xs) -> LogMessage Info (read timeStamp) (unwords xs)
    ("W":timeStamp:xs) -> LogMessage Warning (read timeStamp) (unwords xs)
    _ -> Unknown string

parse :: String -> [LogMessage]
parse logFile = parseLines (lines logFile)

parseLines :: [String] -> [LogMessage]
parseLines [] = []
parseLines (x:xs) = parseMessage x:parseLines xs

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert logMessage Leaf = Node Leaf logMessage Leaf
insert logMessage@(LogMessage _ newTimeStamp _) (Node leftNode oldLogMessage@(LogMessage _ oldTimeStamp _) rightNode)
  | newTimeStamp < oldTimeStamp = Node (insert logMessage leftNode) oldLogMessage rightNode
  | otherwise = Node leftNode oldLogMessage (insert logMessage rightNode)

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x (build xs)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder Node leftNode logMessage rightNode = inOrder leftNode ++ [logMessage] ++ inOrder rightNode
