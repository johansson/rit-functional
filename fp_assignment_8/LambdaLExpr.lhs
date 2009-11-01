This module defines Symbol and LExpr and implements show.

> module LambdaLExpr where
> import LambdaTypes

A Symbol is a string which can be read and shown
as alphanumerics without quotes.

> newtype Symbol = Symbol String deriving Eq

> instance Show Symbol where
>   showsPrec _ (Symbol s) = (s ++)

A LExpr is a SExpr which can be read and shown in Scheme-like notation
and dumped in Haskell notation.

> newtype LExpr a = L (SExpr a) deriving Eq

> instance (Eq a, Show a) => Show (LExpr a) where
>   showsPrec _ (L(expr)) = showsLExpr expr where
>     showsLExpr (Name name) = shows name
>     showsLExpr (Proc name body) =
>       ("(lambda ("++) . shows name . (") "++) . showsLExpr body . (')':)
>     showsLExpr (Call fun arg) =
>       ('(':) . showsLExpr fun . (' ':) . showsLExpr arg . (')':)

dump s-expression
unwraps L() which is a newtype wrapper for SExpr.

- dump :: LExpr t -> SExpr t

> dump (L expr) = expr
