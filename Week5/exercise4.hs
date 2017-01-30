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
