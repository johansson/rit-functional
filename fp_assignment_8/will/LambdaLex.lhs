This module implements readsSymbol and readsLExpr based on lex.

> module LambdaLex where
> import Data.Char
> import LambdaLExpr 
> import LambdaTypes

read alphanumeric symbol

- readsSymbol :: ReadS Symbol

> readsSymbol s = case lex s of
>   [(a,b)] -> if all isAlphaNum a then [(Symbol a, b)] else []
>   _       -> []

read s-expression in Scheme notation

- readsLExpr :: (Eq a, Read a, Show a) => ReadS (LExpr a)

> readsLExpr s = [(L$Proc name body, tail) | ("(",      t1) <- lex s,
>                                            ("lambda", t2) <- lex t1,
>                                            ("(",      t3) <- lex t2,
>                                            (name,     t4) <- reads t3,
>                                            (")",      t5) <- lex t4,
>                                            (L(body),  t6) <- readsLExpr t5,
>                                            (")",      tail) <- lex t6]
>                ++
>                [(L$Call fun arg, tail)   | ("(",      t1) <- lex s,
>                                            (L(fun),   t2) <- readsLExpr t1,
>                                            (L(arg),   t3) <- readsLExpr t2,
>                                            (")",      tail) <- lex t3]
>                ++
>                [(L$Name name, tail)      | (name,     tail) <- reads s]
