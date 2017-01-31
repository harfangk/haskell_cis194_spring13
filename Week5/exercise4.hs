{-
Exercise 4

The marketing department has gotten wind of just how flexible
the calculator project is and has promised custom calculators to some
big clients. As you noticed after the initial roll-out, everyone loves the
interface, but everyone seems to have their own opinion on what the
semantics should be. Remember when we wrote ExprT and thought
that addition and multiplication of integers was pretty cut and dried?
Well, it turns out that some big clients want customized calculators
with behaviors that they have decided are right for them.

The point of our Expr type class is that we can now write down
arithmetic expressions once and have them interpreted in various
ways just by using them at various types.

Make instances of Expr for each of the following types:
• Integer — works like the original calculator
• Bool — every literal value less than or equal to 0 is interpreted
         as False, and all positive Integers
         are interpreted as True; “addition” is logical or,
         “multiplication” is logical and
• MinMax — “addition” is taken to be the max function, while
           “multiplication” is the min function
• Mod7 — all values should be in the ranage 0 . . . 6, and
         all arithmetic is done modulo 7; for example, 5 + 3 = 1.

The last two variants work with Integers internally, but in order
to provide different instances, we wrap those Integers in newtype
wrappers. These are used just like the data constructors we’ve seen
before.

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

Once done, the following code should demonstrate our family of
calculators:

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

Try printing out each of those tests in ghci to see if things are
working. It’s great how easy it is for us to swap in new semantics for
the same syntactic expression!
-}

import ExprT
import Parser

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y) = (eval x) + (eval y)
eval (Mul x y) = (eval x) * (eval y)

evalStr :: String -> Maybe Integer
evalStr x =
  case parsedExp of
    Just exp -> Just (eval exp)
    _ -> Nothing
  where parsedExp = parseExp Lit Add Mul x

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit x = Lit x
  add x y = Add x y
  mul x y = Mul x y

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
