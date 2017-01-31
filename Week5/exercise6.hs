{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-
Exercise 6 (do this OR exercise 5)

Some users of your calculator have requested the ability to give
names to intermediate values and then reuse these stored values
later.

To enable this, you first need to give arithmetic expressions the
ability to contain variables. Create a new type class HasVars a which
contains a single method var :: String -> a. Thus, types which are
instances of HasVars have some notion of named variables.

Start out by creating a new data type VarExprT which is the same
as ExprT but with an extra constructor for variables. Make VarExprT
an instance of both Expr and HasVars. You should now be able to
write things like

*Calc> add (lit 3) (var "x") :: VarExprT

But we can’t stop there: we want to be able to interpret expressions
containing variables, given a suitable mapping from variables
to values. For storing mappings from variables to values, you should
use the Data.Map module. Add

import qualified Data.Map as M

at the top of your file. The qualified import means that you must
prefix M. whenever you refer to things from Data.Map. This is standard
practice, since Data.Map exports quite a few functions with
names that overlap with names from the Prelude. Consult the
Data.Map documentation to read about the operations that are supported
on Maps. 

Implement the following instances:

instance HasVars (M.Map String Integer -> Maybe Integer)
instance Expr (M.Map String Integer -> Maybe Integer)

The first instance says that variables can be interpreted as functions
from a mapping of variables to Integer values to (possibly)
Integer values. It should work by looking up the variable in the
mapping.

The second instance says that these same functions can be interpreted
as expressions (by passing along the mapping to subexpressions
and combining results appropriately).

Note: to write these instances you will need to enable the FlexibleInstances
language extension by putting

{-# LANGUAGE FlexibleInstances #-}

as the first line in your file.

Once you have created these instances, you should be able to test
them as follows:

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs

*Calc> :t add (lit 3) (var "x")
add (lit 3) (var "x") :: (Expr a, HasVars a) => a

*Calc> withVars [("x", 6)] $ add (lit 3) (var "x")
Just 9

*Expr> withVars [("x", 6)] $ add (lit 3) (var "y")
Nothing

*Calc> withVars [("x", 6), ("y", 3)] $ mul (var "x") (add (var "y") (var "x"))
Just 54
-}

import ExprT
import Parser
import StackVM
import qualified Data.Map as M
import Data.Maybe

eval :: ExprT -> Integer
eval (ExprT.Lit x) = x
eval (ExprT.Add x y) = (eval x) + (eval y)
eval (ExprT.Mul x y) = (eval x) * (eval y)

evalStr :: String -> Maybe Integer
evalStr x =
  case parsedExp of
    Just exp -> Just (eval exp)
    _ -> Nothing
  where parsedExp = parseExp ExprT.Lit ExprT.Add ExprT.Mul x

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = ExprT.Lit
  add x y = ExprT.Add x y
  mul x y = ExprT.Mul x y

instance Expr Integer where
  lit x = x
  add = (+) 
  mul = (*)

instance Expr Bool where
  lit x  
    | x > 0 = True
    | otherwise = False
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit x = MinMax x
  add (MinMax x) (MinMax y) = lit (max x y)
  mul (MinMax x) (MinMax y) = lit (min x y)

instance Expr Mod7 where
  lit x = Mod7 (x `mod` 7)
  add (Mod7 x) (Mod7 y) = lit (x + y)
  mul (Mod7 x) (Mod7 y) = lit (x * y)

instance Expr StackVM.Program where
  lit x = [StackVM.PushI x]
  add x y = x ++ y ++ [StackVM.Add]
  mul x y = x ++ y ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul

class HasVars a where
  var :: String -> a

data VarExprT = VarLit Integer
              | VarAdd VarExprT VarExprT
              | VarMul VarExprT VarExprT
              | Var String
              deriving (Show, Eq)

instance Expr VarExprT where
  lit = VarLit 
  add = VarAdd 
  mul = VarMul

instance HasVars VarExprT where
  var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var s = M.lookup s

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit x = \_ -> Just x
  add f g = \x -> case (isNothing (f x)) || (isNothing (g x)) of
    True -> Nothing
    _  -> Just (fromJust (f x) + fromJust (g x))
  mul f g = \x -> case (isNothing (f x)) || (isNothing (g x)) of
    True -> Nothing
    _  -> Just (fromJust (f x) * fromJust (g x))

withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
