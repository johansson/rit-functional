Authors: Axel Schreiner and Joseph Pecoraro
Date: Monday October 19, 2009
Description: Function Programming Assignment #6 and Function Programming Assignment #5 Solution
Haskell

> module LambdaTypes where
> import Symbol


Macros
------

> type Macros a = [(a, SExpr a)]


SExpr Data Type
---------------

A raw SExpr Data Type including the fundamentals
  - Name = variable
  - Proc = procedure declaration
  - Call = invocation

> data (Eq a, Show a) => SExpr a =
>   Name a |
>   Proc a (SExpr a) |
>   Call (SExpr a) (SExpr a)
>   deriving (Eq, Show)


LExpr - Thin Wrapper
--------------------

LExpr wraps an SExpr with Show/Read characteristics.

> newtype LExpr a = L (SExpr a) deriving (Eq)


  Showing a Lambda.
    Name "x"     => x
    Proc "x" y   => (lambda (x) y)
    Call x y     => (x y)

> instance (Eq a, Show a) => Show (LExpr a) where
>   showsPrec _ (L x) = showsSExpr x

> showsSExpr (Name a)        = shows a
> showsSExpr (Proc a body)   = ("(lambda ("++) . shows a . (") "++) . showsSExpr body . (')':)
> showsSExpr (Call expr arg) = ('(':) . showsSExpr expr . (' ':) . showsSExpr arg . (')':)



  Reading a Lambda.
  usage: ghci> readsSExpr "(lambda (x) y)" :: [(SExpr Symbol, String)]
         ghci> read "(lambda (x) y)" :: LExpr Symbol
         ghci> dump $ (read "(lambda (x) y)" :: LExpr Symbol) --$ hilite fix --

> instance (Eq a, Show a, Read a) => Read (LExpr a) where
>   readsPrec _ s = map (\(a,b) -> (L a, b)) (readsSExpr s)

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

