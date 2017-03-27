module Party where

import Data.Tree
import Employee

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
