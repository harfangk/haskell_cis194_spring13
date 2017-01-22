{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

{-
Exercise 1

The first step is figuring out how to parse an individual message.
Define a function

parseMessage :: String -> LogMessage

which parses an individual line from the log file. For example,

parseMessage "E 2 562 help help"
== LogMessage (Error 2) 562 "help help"

parseMessage "I 29 la la la"
== LogMessage Info 29 "la la la"

parseMessage "This is not in the right format"
== Unknown "This is not in the right format"

Once we can parse one log message, we can parse a whole log file.
Define a function

parse :: String -> [LogMessage]

which parses an entire log file at once and returns its contents as a
list of LogMessages.

To test your function, use the testParse function provided in the
Log module, giving it as arguments your parse function, the number
of lines to parse, and the log file to parse from (which should also be
in the same folder as your assignment). For example, after loading
your assignment into GHCi, type something like this at the prompt:

testParse parse 10 "error.log"

Don’t reinvent the wheel! (That’s so last week.) Use Prelude functions
to make your solution as concise, high-level, and functional as
possible. For example, to convert a String like "562" into an Int, you
can use the read function. Other functions which may (or may not)
be useful to you include lines, words, unwords, take, drop, and (.).

Putting the logs in order

Unfortunately, due to the error messages being generated by multiple
servers in multiple locations around the globe, a lightning storm, a
failed disk, and a bored yet incompetent programmer, the log messages
are horribly out of order. Until we do some organizing, there
will be no way to make sense of what went wrong! We’ve designed a
data structure that should help—a binary search tree of LogMessages:

data MessageTree = Leaf
| Node MessageTree LogMessage MessageTree

Note that MessageTree is a recursive data type: the Node constructor
itself takes two children as arguments, representing the left and
right subtrees, as well as a LogMessage. Here, Leaf represents the
empty tree.

A MessageTree should be sorted by timestamp: that is, the timestamp
of a LogMessage in any Node should be greater than all timestamps
of any LogMessage in the left subtree, and less than all timestamps
of any LogMessage in the right child.

Unknown messages should not be stored in a MessageTree since
they lack a timestamp
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
