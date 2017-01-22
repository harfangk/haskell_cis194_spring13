{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

{-
Exercise 2

Define a function

insert :: LogMessage -> MessageTree -> MessageTree

which inserts a new LogMessage into an existing MessageTree, producing
a new MessageTree. insert may assume that it is given a
sorted MessageTree, and must produce a new sorted MessageTree
containing the new LogMessage in addition to the contents of the
original MessageTree.

However, note that if insert is given a LogMessage which is
Unknown, it should return the MessageTree unchanged.
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
