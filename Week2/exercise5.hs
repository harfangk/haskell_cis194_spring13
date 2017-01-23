{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

{-
Exercise 5 

Now that we can sort the log messages, the only thing
left to do is extract the relevant information. We have decided that
“relevant” means “errors with a severity of at least 50”.

Write a function

whatWentWrong :: [LogMessage] -> [String]

which takes an unsorted list of LogMessages, and returns a list of the
messages corresponding to any errors with a severity of 50 or greater,
sorted by timestamp. (Of course, you can use your functions from the
previous exercises to do the sorting.)

For example, suppose our log file looked like this:

I 6 Completed armadillo processing
I 1 Nothing to report
E 99 10 Flange failed!
I 4 Everything normal
I 11 Initiating self-destruct sequence
E 70 3 Way too many pickles
E 65 8 Bad pickle-flange interaction detected
W 5 Flange is due for a check-up
I 7 Out for lunch, back in two time steps
E 20 2 Too many pickles
I 9 Back from lunch

This file is provided as sample.log. There are four errors, three of
which have a severity of greater than 50. The output of whatWentWrong
on sample.log ought to be

[ "Way too many pickles"
, "Bad pickle-flange interaction detected"
, "Flange failed!"
]

You can test your whatWentWrong function with testWhatWentWrong,
which is also provided by the Log module. You should provide
testWhatWentWrong with your parse function, your whatWentWrong
function, and the name of the log file to parse.
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
inOrder (Node leftNode logMessage rightNode) = (inOrder leftNode) ++ [logMessage] ++ (inOrder rightNode)

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
