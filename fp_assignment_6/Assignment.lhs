Author: Joseph Pecoraro
Date: Monday October 19, 2009
Description: Function Programming Assignment #6
Haskell

> module Assignment where
> import Symbol
> import LambdaTypes
> import LambdaOps


run a series of statements
--------------------------

  Uses a helper function:

    r <macros> <results> <statementsLeftToProcess>
  
  A Macro Statement is one that contains a "=" as its second token:
  
    " true       = (lambda (x) (lambda (y) x)) "

  and becomes an entry into Macros macros as:
  
    (Symbol "true", (lambda (x) (lambda (y) x)))

  A Statement is processed with the Professor's library.

> run statements = r [] [] statements
>   where r _      results [] = results
>         r macros results (x:xs) = case isMacroStatement x of
>             True  -> r (((macroSymbol x), (sexprMacro x)):macros) results xs
>             False -> r macros (results++[sexprGeneral x macros]) xs

>         -- Helpers for reading particular tokens / string sections
>         firstToken  s      = fst.head $ lex s
>         afterEquals s      = snd.head $ lex.snd.head $ lex s
>         isMacroStatement s = (=="=").fst.head $ lex.snd.head $ lex s

>         -- High level helpers used above
>         macroSymbol  s   = Symbol (firstToken s)
>         sexprMacro   s   = read (afterEquals s) :: LExpr Symbol
>         sexprGeneral s m = rswab'ul m s


Testing Code
------------

> -- All the macros for convienience
> macros     = [" true =  (lambda (x) (lambda (y) x)) ",
>               " false = (lambda (x) (lambda (y) y)) ",
>               " ifThenElse = (lambda (cond) (lambda (then) (lambda (else) ((cond then) else)))) ",
>               " 0 = (lambda (f) (lambda (x) x)) ",
>               " 1 = (lambda (f) (lambda (x) (f x))) ",
>               " 2 = (lambda (f) (lambda (x) (f (f x)))) ",
>               " 3 = (lambda (f) (lambda (x) (f (f (f x))))) ",
>               " repeat = (lambda (n) (lambda (x) ((n (lambda (g) (g x))) (lambda (y) y)))) ",
>               " succ = (lambda (n) (lambda (f) (lambda (x) (f ((n f) x))))) ",
>               " pred = (lambda (n) (((n (lambda (p) (lambda (z) ((z (succ (p true))) (p true))))) (lambda (z) ((z 0) 0))) false)) ",
>               " sum = (lambda (m) (lambda (n) (lambda (f) (lambda (x) ((m f) ((n f) x)))))) ",
>               " product = (lambda (m) (lambda (n) (lambda (f) (m (n f))))) ",
>               " isZero = (lambda (n) ((n (lambda (x) false)) true)) ",
>               " G = (lambda (f) (lambda (n) (((ifThenElse (isZero n)) 1) ((product n) (f (pred n)))))) ",
>               " Y = (lambda (y) ((lambda (x) (y (x x))) (lambda (x) (y (x x))))) ",
>               " factorial = (Y G) "]


> -- Basic ifThenElse statements [PASS]
> s1 = [" (((ifThenElse true)  this) that) ",
>       " (((ifThenElse false) this) that) "]


> -- Basic Church Numerals [PASS]
> s2 = [" ((0 (lambda (f) (f hello))) (lambda (x) x)) ",
>       " ((1 (lambda (f) (f hello))) (lambda (x) x)) ",
>       " ((2 (lambda (f) (f hello))) (lambda (x) x)) ",
>       " ((3 (lambda (f) (f hello))) (lambda (x) x)) "]


> -- Arithmetic [PASS]
> s3 = [" ((repeat 2) hello) ",
>       " ((repeat (succ 2)) hello) ",
>       " ((repeat (pred 2)) hello) ",
>       " ((repeat ((sum 2) 3)) hello) ",
>       " ((repeat ((product 2) 3)) hello) "]

> -- Boolean Test [PASS]
> s4 = [" (((ifThenElse (isZero 0)) this) that) ",
>       " (((ifThenElse (isZero 1)) this) that) " ]

> -- Recursive Test [...]
> s5 = [" ((repeat (factorial ((product 2) 2))) hello) "]

> -- Combined
> ms1 = macros ++ s1
> ms2 = macros ++ s2
> ms3 = macros ++ s3
> ms4 = macros ++ s4
> ms5 = macros ++ s5


Tests for the sets of statements:
@ run ms1 == (read "[this, that]"::[LExpr Symbol])
@ run ms2 == (read "[(lambda (x) x), hello, (hello hello), ((hello hello) hello)]"::[LExpr Symbol])
@ run ms3 == (read "[(hello hello),((hello hello) hello),hello,((((hello hello) hello) hello) hello),(((((hello hello) hello) hello) hello) hello)]"::[LExpr Symbol])
@ run ms4 == (read "[this, that]"::[LExpr Symbol])

This one takes a while:
@ run ms5 == (read "[(((((((((((((((((((((((hello hello) hello) hello) hello) hello) hello) hello) hello) hello) hello) hello) hello) hello) hello) hello) hello) hello) hello) hello) hello) hello) hello) hello)]"::[LExpr Symbol])
