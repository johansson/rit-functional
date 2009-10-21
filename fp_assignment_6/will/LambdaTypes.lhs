> -- Will's implementation

This module is designed to be replaced by location in a particular directory.

> module LambdaTypes where

The following constructs need to be represented:

> data (Eq a, Show a) => SExpr a =
>   {- variable -}         Name a |
>   {- procedure -}        Proc a (SExpr a) |
>   {- invocation -}       Call (SExpr a) (SExpr a)
>   deriving (Eq)

> type Macros a = [(a, SExpr a)]

where the macro call cannot be distinguished from a variable.

> newtype Symbol = Symbol String deriving Eq
> newtype LExpr a = L (SExpr a) deriving Eq
>
> instance Show Symbol where
>	show (Symbol name) = name
>
> instance (Eq a, Show a) => Show (SExpr a) where
>	showsPrec _ x = showsSExpr x
>
> -- same thing as SExpr, really, just extract the SExpr from the LExpr part
> -- and we're on our way.
> instance (Eq a, Show a) => Show (LExpr a) where
>   showsPrec _ (L x) = showsSExpr x
>
> -- Clearly, self documenting:
> showsSExpr (Name a) = shows a
> -- (Proc a body) is (lambda (a) b)
> showsSExpr (Proc a b)   = ("(lambda ("++) . shows a . (") "++) . showsSExpr b . (')':)
> -- (Call a b) is (a b)
> showsSExpr (Call a b) = ('(':) . showsSExpr a . (' ':) . showsSExpr b . (')':)

> instance Read (Symbol) where
>   readsPrec _ s = [(Symbol a, x) | (a, x) <- lex s]

> instance (Eq a, Show a, Read a) => Read (LExpr a) where
>   readsPrec _ s = map (\(a,b) -> (L a, b)) (readsSExpr s)

> -- shamefully borrowed from joe, because i was trying to
> -- figure out why [(Name a, x) | (a,x) <- reads s]
> -- wasn't working, even though I had the same thing
> -- at first, even with joe's code, i still get
> -- [(somename,"")] rather than [(Name somename,"")]
> -- okay, joe explained that my SExpr is actually more like his LExpr
> -- and it's behaving correctly, and i wouldnt want Name anyway
> -- since we're trying to be as close to Scheme's lambda calc
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

> convertToSymbols :: SExpr String -> SExpr Symbol
> convertToSymbols (Name a)        = Name (Symbol a)
> convertToSymbols (Proc a body)   = Proc (Symbol a) (convertToSymbols body)
> convertToSymbols (Call expr arg) = Call (convertToSymbols expr) (convertToSymbols arg)

> convertFromSymbols :: SExpr Symbol -> SExpr String
> convertFromSymbols (Name (Symbol a))        = Name a
> convertFromSymbols (Proc (Symbol a) body)   = Proc a (convertFromSymbols body)
> convertFromSymbols (Call expr arg)          = Call (convertFromSymbols expr) (convertFromSymbols arg)