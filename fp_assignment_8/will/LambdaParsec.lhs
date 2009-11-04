This module implements readsSymbol and readsLExpr based on parsec.

> module LambdaParsec where
> import LambdaLExpr 
> import LambdaTypes
> import Text.ParserCombinators.Parsec
> import Text.ParserCombinators.Parsec.Pos

read alphanumeric symbol

The tricky part is to retrieve the unparsed input.
This can be done with the state accessor getInput.

- readsSymbol :: ReadS Symbol
- atom :: GenParser Char st (Symbol, [Char])

> readsSymbol s = case parse atom "" s of
>     Left _  -> []
>     Right x -> [x]
>   where
>     atom = do spaces
>               name <- many1 alphaNum
>               spaces
>               tail <- getInput
>               return (Symbol name, tail)

read s-expression in Scheme notation

> readsLExpr :: (Eq a, Show a, Read a) => ReadS (LExpr a)
> --lexpr :: (Eq a, Show a, Read a) => GenParser Char st ((LExpr a), String)

The tricky part is to avoid binding this to Symbol.
Once the types are declared, the following parser leaves it unbound:

> readsLExpr s = case parse lexpr "" s of
>     Left _  -> []
>     Right x -> [x]
>   where
>     lexpr = do spaces       -- top-level leading spaces
>                expr <- lexp -- expected to skip trailing spaces
>                tail <- getInput
>                return (expr, tail)

>     lexp = do char '(' -- need to factor leading (
>               spaces
>               e <- procOrCall
>               char ')'
>               spaces
>               return e
>            <|>
>            do name <- many1 alphaNum
>               spaces
>               return (L$Name (read name))

>     procOrCall = do string "lambda"
>                     spaces
>                     char '('
>                     spaces
>                     name <- many1 alphaNum
>                     spaces
>                     char ')'
>                     spaces
>                     L(body) <- lexp
>                     return (L$Proc (read name) body)
>                   <|>      
>                   do L(fun) <- lexp
>                      L(arg) <- lexp
>                      return (L$Call fun arg)
