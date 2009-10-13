Lambda Calculus Interpreter

Will Johansson, Joseph Pecoraro

Definition stolen from Professor's solution for Week 4.

> module Lambda where
> data (Eq a, Show a) => SExpr a =
>   {- variable -}         Name a |
>   {- procedure -}        Proc a (SExpr a) |
>   {- invocation -}       Call (SExpr a) (SExpr a) |
>   {- macro -}            Macro a (SExpr a)
>   deriving (Eq, Show)
> type Macros = [(String, SExpr String)]

Substitute a macro.

We'll need to go through the list (list comprehension?)
to match whatever that's in SExpr, but SExpr is possibly
recursive, for example:

true = Macro "true" (Proc "x" (Proc "y" (Name "x")))
false = Macro "false" (Proc "x" (Proc "y" (Name "y")))
ifThenElse = Macro "if-then-else" (Proc "cond" (Proc "then" (Proc "else" (Call (Call (Name "cond") (Name "then")) (Name "else")))))

booleanThis = Call (Call (Call (Name "if-then-else") (Name "true")) (Name "this")) (Name "that")

Clearly we'll need to substitute (Name "if-then-else").

booleanThis = Call (Call (Call (Proc "cond" (Proc "then" (Proc "else" (Call (Call (Name "cond") (Name "then")) (Name "else"))))))) /* FIX ME, I forgot this / that, just fix it later. You get the point */

> substitute :: Macros -> SExpr String -> SExpr String 
> substitute a b = b

Do beta reductions.

> beta :: SExpr String -> SExpr String 
> beta a = a