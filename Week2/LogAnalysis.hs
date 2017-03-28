{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

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
parseLines = map parseMessage

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert logMessage Leaf = Node Leaf logMessage Leaf
insert logMessage@(LogMessage _ newTimeStamp _) (Node leftNode oldLogMessage@(LogMessage _ oldTimeStamp _) rightNode)
  | newTimeStamp < oldTimeStamp = Node (insert logMessage leftNode) oldLogMessage rightNode
  | otherwise = Node leftNode oldLogMessage (insert logMessage rightNode)

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf 

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node leftNode logMessage rightNode) = inOrder leftNode ++ [logMessage] ++ inOrder rightNode

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong list = (extractMessages . inOrder . build . filterSevereErrors) list

filterSevereErrors :: [LogMessage] -> [LogMessage]
filterSevereErrors [] = []
filterSevereErrors (x:xs) =
  case x of
    LogMessage (Error severity) _ _
      | severity > 50 -> x:filterSevereErrors xs
      | otherwise -> filterSevereErrors xs
    _ -> filterSevereErrors xs

extractMessages :: [LogMessage] -> [String]
extractMessages [] = []
extractMessages (x:xs) = 
  case x of
    LogMessage _ _ message -> message : extractMessages xs
    _ -> extractMessages xs
