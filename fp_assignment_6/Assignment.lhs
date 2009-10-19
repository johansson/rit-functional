Author: Joseph Pecoraro
Date: Monday October 19, 2009
Description: Function Programming Assignment #6
Haskell

> module Assignment where
> import Symbol
> import LambdaTypes
> import Lambda


run a series of statements
--------------------------

  Uses a helper function:

    r <macros> <results> <statementsLeftToProcess>
  
  A Macro Statement is one that contains a "=" as its second token:
  
    " true       = (lambda (x) (lambda (y) x)) "

  and becomes an entry into Macros macros as:
  
    (Symbol "true", (lambda (x) (lambda (y) x)))

  A Statement is processed as the Professor's library shows:
  
    1. Substitute macros in the equation
    2. Apply alpha
    3. Uncurry Beta
    4. Unwrap

> run statements = r [] [] statements
>   where r _      results [] = results
>         r macros results (x:xs) = case isMacroStatement x of
>             True  -> r (((macroSymbol x), (sexprMacro x)):macros) results xs
>             False -> r macros (results++[sexprGeneral x macros]) xs

>         -- Helpers for reading particular tokens / string sections
>         firstToken s       = fst.head $ lex s
>         afterEquals s      = snd.head $ lex.snd.head $ lex s
>         isMacroStatement s = (=="=").fst.head $ lex.snd.head $ lex s

>         -- Low level read and process
>         read' str  = read str :: SExpr Symbol
>         eval sexpr = unwrap $ (\(x, _, _) -> x) $ uncurry beta $ alpha [] 0 $ wrap $ sexpr

>         -- High level helpers used above
>         macroSymbol s    = Symbol (firstToken s)
>         sexprMacro  s    = read' $ afterEquals s
>         sexprGeneral s m = eval $ substitute m $ read' s


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
>               " isZero = (lambda (n) ((n (lambda (x) false)) true)) "]

> -- Basic ifThenElse statements [PASS]
> s1 = macros ++ [" (((ifThenElse true)  this) that) ",
>                 " (((ifThenElse false) this) that) "]

> -- Basic Church Numerals [PASS]
> s2 = macros ++ [" ((0 (lambda (f) (f hello))) (lambda (x) x)) ",
>                 " ((1 (lambda (f) (f hello))) (lambda (x) x)) ",
>                 " ((2 (lambda (f) (f hello))) (lambda (x) x)) ",
>                 " ((3 (lambda (f) (f hello))) (lambda (x) x)) "]


> -- Arithmetic [????? Parses but doesn't fully reduce?]
> s3 = macros ++ [" ((repeat 2) hello) ",
>                 " ((repeat (succ 2)) hello) ",
>                 " ((repeat (pred 2)) hello) ",
>                 " ((repeat ((sum 2) 3)) hello) ",
>                 " ((repeat ((product 2) 3)) hello) "]

> -- Boolean Test [???? Parses but doesn't fully reduce?]
> s4 = macros ++ [" (((ifThenElse (isZero 0)) this) that) ",
>                 " (((ifThenElse (isZero 1)) this) that) " ]


Tests for set of statements (s1) and (s2):
@ run s1 == (read "[this, that]"::[SExpr Symbol])
@ run s2 == (read "[(lambda (x) x), hello, (hello hello), ((hello hello) hello)]"::[SExpr Symbol])

