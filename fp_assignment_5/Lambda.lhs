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

> type Macros a = [Macro a]
> data Macro a =  Tag [Macros a]
> substitute :: Macros -> SExpr String -> SExpr String 
> substitute a b = b
> beta :: SExpr String -> SExpr String 
> beta a = a