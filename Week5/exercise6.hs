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

data ExprT = Lit Integer
           | Add ExprT ExprT
           | Mul ExprT ExprT
           deriving (Show, Eq)

eval :: ExprT -> Integer

evalStr :: String -> Maybe Integer

class Expr a where
  lit :: a -> Integer 
  add :: a -> a -> Integer
  mul :: a -> a -> Integer

instance Expr ExprT where
  lit x =
  add x y =
  mul x y = 
