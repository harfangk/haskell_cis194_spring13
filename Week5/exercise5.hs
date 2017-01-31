{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-
Exercise 5 (do this OR exercise 6)

The folks down in hardware have finished our new custom CPU,
so we’d like to target that from now on. The catch is that a stackbased
architecture was chosen to save money. You need to write a
version of your calculator that will emit assembly language for the
new processor.

The hardware group has provided you with StackVM.hs, which
is a software simulation of the custom CPU. The CPU supports six
operations, as embodied in the StackExp data type:

data StackExp = PushI Integer
              | PushB Bool
              | Add
              | Mul
              | And
              | Or
              deriving Show
type Program = [StackExp]

PushI and PushB push values onto the top of the stack, which can
store both Integer and Bool values. Add, Mul, And, and Or each pop
the top two items off the top of the stack, perform the appropriate
operation, and push the result back onto the top of the stack. For
example, executing the program

[PushB True, PushI 3, PushI 6, Mul]

will result in a stack holding True on the bottom, and 18 on top of
that.

If there are not enough operands on top of the stack, or if an operation
is performed on operands of the wrong type, the processor
will melt into a puddle of silicon goo. For a more precise specification
of the capabilities and behavior of the custom CPU, consult the
reference implementation provided in StackVM.hs.

Your task is to implement a compiler for arithmetic expressions.
Simply create an instance of the Expr type class for Program, so that
arithmetic expressions can be interpreted as compiled programs. For
any arithmetic expression exp :: Expr a => a it should be the case
that

stackVM exp == Right [IVal exp]

Note that in order to make an instance for Program (which is a
type synonym) you will need to enable the TypeSynonymInstances
language extension, which you can do by adding

{-# LANGUAGE TypeSynonymInstances #-}

as the first line in your file.

Finally, put together the pieces you have to create a function

compile :: String -> Maybe Program

which takes Strings representing arithmetic expressions and compiles
them into programs that can be run on the custom CPU.
-}

import ExprT
import Parser
import StackVM

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
  lit x = ExprT.Lit x
  add x y = ExprT.Add x y
  mul x y = ExprT.Mul x y

instance Expr Integer where
  lit x = x
  add x y = x + y
  mul x y = x * y

instance Expr Bool where
  lit x  
    | x > 0 = True
    | otherwise = False
  add x y = x || y
  mul x y = x && y

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
