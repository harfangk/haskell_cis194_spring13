{-
Exercise 2

The UI department has internalized the focus group data and is
ready to synergize with you. They have developed the front-facing
user-interface: a parser that handles the textual representation of the
selected language. They have sent you the module Parser.hs, which
exports parseExp, a parser for arithmetic expressions. If you pass
the constructors of ExprT to it as arguments, it will convert Strings
representing arithmetic expressions into values of type ExprT. For
example:

*Calc> parseExp Lit Add Mul "(2+3)*4"
Just (Mul (Add (Lit 2) (Lit 3)) (Lit 4))

*Calc> parseExp Lit Add Mul "2+3*4"
Just (Add (Lit 2) (Mul (Lit 3) (Lit 4)))

*Calc> parseExp Lit Add Mul "2+3*"
Nothing

Leverage the assets of the UI team to implement the value-added
function

evalStr :: String -> Maybe Integer

which evaluates arithmetic expressions given as a String, producing
Nothing for inputs which are not well-formed expressions, and
Just n for well-formed inputs that evaluate to n.
-}

data ExprT = Lit Integer
           | Add ExprT ExprT
           | Mul ExprT ExprT
           deriving (Show, Eq)

eval :: ExprT -> Integer

evalStr :: String -> Maybe Integer
