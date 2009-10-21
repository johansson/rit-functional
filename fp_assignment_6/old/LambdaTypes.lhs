Authors: Axel Schreiner and Joseph Pecoraro
Date: Monday October 19, 2009
Description: Function Programming Assignment #6 and Function Programming Assignment #5 Solution
Haskell

> module LambdaTypes where
> import Symbol


Macros
------

> type Macros a = [(a, SExpr a)]


Lambdas
-------

> data (Eq a, Show a, Read a) => SExpr a =
>   Name a |
>   Proc a (SExpr a) |
>   Call (SExpr a) (SExpr a)
>   deriving (Eq)

  Showing a Lambda.
    Name "x"     => x
    Proc "x" y   => (lambda (x) y)
    Call x y     => (x y)

> instance (Eq a, Show a, Read a) => Show (SExpr a) where
>   showsPrec _ x = showsSExpr x

> showsSExpr (Name a)        = shows a
> showsSExpr (Proc a body)   = ("(lambda ("++) . shows a . (") "++) . showsSExpr body . (')':)
> showsSExpr (Call expr arg) = ('(':) . showsSExpr expr . (' ':) . showsSExpr arg . (')':)

  Reading a Lambda.
  usage: ghci> read "(lambda (x) y)" :: SExpr Symbol
  
> instance (Eq a, Show a, Read a) => Read (SExpr a) where
>   readsPrec _ s = readsSExpr s

> readsSExpr :: (Eq a, Show a, Read a) => ReadS (SExpr a)
> readsSExpr s = [(Proc a body, x)    | ("(", t)      <- lex s,
>                                       ("lambda", u) <- lex t,
>                                       ("(", uu)     <- lex u,
>                                       (a, v)        <- reads uu,
>                                       (")", w)      <- lex v,
>                                       (body, y)     <- readsSExpr w,
>                                       (")", x)      <- lex y ]
>                ++                   
>                [(Name a, x)         | (a, x)        <- reads s]
>                ++
>                [(Call expr args, x) | ("(", t)      <- lex s,
>                                       (expr, u)     <- readsSExpr t,
>                                       (args, v)     <- readsSExpr u,
>                                       (")", x)      <- lex v]


Helpers
-------

> convertToSymbols :: SExpr String -> SExpr Symbol
> convertToSymbols (Name a)        = Name (Symbol a)
> convertToSymbols (Proc a body)   = Proc (Symbol a) (convertToSymbols body)
> convertToSymbols (Call expr arg) = Call (convertToSymbols expr) (convertToSymbols arg)

> convertFromSymbols :: SExpr Symbol -> SExpr String
> convertFromSymbols (Name (Symbol a))        = Name a
> convertFromSymbols (Proc (Symbol a) body)   = Proc a (convertFromSymbols body)
> convertFromSymbols (Call expr arg)          = Call (convertFromSymbols expr) (convertFromSymbols arg)

