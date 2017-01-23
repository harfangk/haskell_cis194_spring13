{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

{-
Exercise 3

Once we can insert a single LogMessage into a MessageTree,
we can build a complete MessageTree from a list of messages. Specifi-
cally, define a function

build :: [LogMessage] -> MessageTree

which builds up a MessageTree containing the messages in the list,
by successively inserting the messages into a MessageTree (beginning
with a Leaf).
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
parseLines [x] = [parseMessage x]
parseLines (x:xs) = parseMessage x:parseLines xs

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert logMessage Leaf = Node Leaf logMessage Leaf
insert logMessage@(LogMessage _ newTimeStamp _) (Node leftNode oldLogMessage@(LogMessage _ oldTimeStamp _) rightNode)
  | newTimeStamp < oldTimeStamp = Node (insert logMessage leftNode) oldLogMessage rightNode
  | otherwise = Node leftNode oldLogMessage (insert logMessage rightNode)
