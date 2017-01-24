{-  
Exercise 1 

Hopscotch

Your first task is to write a function

skips :: [a] -> [[a]]

The output of skips is a list of lists. The first list in the output should
be the same as the input list. The second list in the output should
contain every second element from the input list. . . and the nth list in
the output should contain every nth element from the input list.
For example:

skips "ABCD" == ["ABCD", "BD", "C", "D"]
skips "hello!" == ["hello!", "el!", "l!", "l", "o", "!"]
skips [1] == [[1]]
skips [True,False] == [[True,False], [False]]
skips [] == []

Note that the output should be the same length as the input.
-}

skips :: [a] -> [[a]]
skips [] = []
skips xs = runEveryNth [1..(length xs)] xs

runEveryNth :: [Int] -> [a] -> [[a]]
runEveryNth [] _ = []
runEveryNth (x:xs) list = everynth x list : runEveryNth xs list

everynth :: Int -> [a] -> [a]
everynth n xs = 
  case drop (n-1) xs of
    [] -> []
    (y:ys) -> y : everynth n ys
