module Party where

import Data.Tree
import Data.Monoid
import Employee
import Data.List

glCons :: Employee -> GuestList -> GuestList
glCons emp@(Emp _ empFun) (GL empList glFun) = GL (emp : empList) (glFun + empFun)

instance Monoid GuestList where
  mappend (GL empList1 fun1) (GL empList2 fun2) = GL (empList1 ++ empList2) (fun1 + fun2)
  mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL empList1 fun1) gl2@(GL empList2 fun2) 
  | fun1 >= fun2 = gl1
  | otherwise = gl2

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node rl sf) = f rl (map (treeFold f) sf)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel emp gl = ((glCons emp $ snd sumGl), (fst sumGl))
  where sumGl = foldl (<>) mempty gl

maxFun :: Tree Employee -> GuestList
maxFun t = moreFun (fst foldedTree) (snd foldedTree)
  where foldedTree = treeFold nextLevel t

getFun :: GuestList -> Fun
getFun (GL _ fun) = fun

getEmployees :: GuestList -> [Employee]
getEmployees (GL el _) = sortBy (\a b -> (empName a) `compare` (empName b)) el

main = do
  tree <- readFile "company.txt"
  let gl = maxFun . read $ tree
      firstLine = "Total fun: " ++ (show . getFun $ gl)
      sortedEmployees = map empName . getEmployees $ gl 
  putStrLn firstLine
  mapM_ print sortedEmployees
