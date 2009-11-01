This module implements readsSymbol and readsLExpr based on Hutton's parsing functions.

> module LambdaParsing where
> import LambdaLExpr 
> import LambdaTypes
> import Monad -- mplus
> import Parsing

read alphanumeric symbol

- readsSymbol :: ParserFunction Symbol
- atom :: Parser Symbol

> readsSymbol = parse atom
> atom = do name <- token (some alphanum)
>           return (Symbol name)

read s-expression in Scheme notation

The tricky part is to avoid binding this to Symbol.
Once the types are declared, using Parser(reads) leaves it unbound.

> readsLExpr :: (Eq a, Show a, Read a) => ParserFunction (LExpr a)
> lexpr :: (Eq a, Show a, Read a) => Parser (LExpr a)

> readsLExpr = parse lexpr
> lexpr = do symbol "("
>            symbol "lambda"
>            symbol "("
>            name <- Parser(reads)
>            symbol ")"
>            L(body) <- lexpr
>            symbol ")"
>            return (L$Proc name body)
>         `mplus`
>         do symbol "("
>            L(fun) <- lexpr
>            L(arg) <- lexpr
>            symbol ")"
>            return (L$Call fun arg)
>         `mplus`
>         do name <- Parser(reads)
>            return (L$Name name)
