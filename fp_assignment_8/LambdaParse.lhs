This module declares read for s-expressions using Scheme notation.
It hides the actual implementation.

> module LambdaParse where
> import LambdaLExpr
> -- import LambdaLex -- uses lex, defines readsSymbol, readsLExpr
> import LambdaParsing -- uses Parsing, defines readsSymbol, readsLExpr
> -- import LambdaParsec -- uses Parsec, defines readsSymbol, readsLExpr
> import LambdaTypes

Symbol

> instance Read Symbol where
>   readsPrec _ = readsSymbol

LExpr

> instance (Eq a, Read a, Show a) => Read (LExpr a) where
>   readsPrec _ = readsLExpr
