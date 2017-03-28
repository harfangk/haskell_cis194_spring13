{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Calc where

import ExprT
import Parser
import StackVM
import qualified Data.Map as M
import Data.Maybe

eval :: ExprT -> Integer
eval (ExprT.Lit x) = x
eval (ExprT.Add x y) = eval x + eval y
eval (ExprT.Mul x y) = eval x * eval y

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
  add = ExprT.Add
  mul = ExprT.Mul 

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
  lit = MinMax
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
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit x _ = Just x
  add f g = if isNothing (f x) || isNothing (g x)
            then Nothing
            else Just (fromJust (f x) + fromJust (g x))
  mul f g = if isNothing (f x) || isNothing (g x)
            then Nothing
            else Just (fromJust (f x) * fromJust (g x))

withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
