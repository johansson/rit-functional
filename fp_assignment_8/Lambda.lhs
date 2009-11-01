This module contains all the usual examples.

> module Main where
> import Data.Char
> import LambdaLExpr
> import LambdaOps
> import LambdaParse
> import LambdaTypes

Required function (not suitable for the examples below).

> run lines = map (rswab'uls defs) rest where
>   defs = macros $ filter ('=' `elem`) lines
>   rest = filter ('=' `notElem`) lines

Main demonstration.

> main = do putStr $ unlines $ map (rswab'uls m8) $ e1 : e2 ++ e3 ++ e4 ++ e5 ++ e6 ++ e7 ++ e8

macros lines
convert macro lines in Scheme notation to Macros.

> macros :: [String] -> LMacros Symbol -- required to coerce reads

> macros lines = [(name, value) | macro       <- lines,
>                                 (name, t1)  <- reads macro,
>                                 ("=",  t2)  <- lex t1,
>                                 (value, t3) <- reads t2]

lexical scoping:

> e1 = " (((lambda (x) (lambda (x) x)) a) b) "

Church booleans:

> m2 = macros [
>       " true =  (lambda (x) (lambda (y) x)) ",
>       " false = (lambda (x) (lambda (y) y)) ",
>       " ifThenElse = (lambda (cond) (lambda (then) (lambda (else) ((cond then) else)))) " ]

> e2 = [" (((ifThenElse true) this) that) ",
>       " (((ifThenElse false) this) that) "]
  
Church numerals:

> m3 = m2 ++ macros [
>       " 0 = (lambda (f) (lambda (x) x)) ",
>       " 1 = (lambda (f) (lambda (x) (f x))) ",
>       " 2 = (lambda (f) (lambda (x) (f (f x)))) ",
>       " 3 = (lambda (f) (lambda (x) (f (f (f x))))) "]

> e3 = [" ((0 (lambda (f) (f hello))) (lambda (x) x)) ",
>       " ((1 (lambda (f) (f hello))) (lambda (x) x)) ",
>       " ((2 (lambda (f) (f hello))) (lambda (x) x)) ",
>       " ((3 (lambda (f) (f hello))) (lambda (x) x)) "]

iteration:

> m4 = m3 ++ macros [
>       " repeat = (lambda (n) (lambda (x) ((n (lambda (g) (g x))) (lambda (y) y)))) "]

> e4 = [" ((repeat 2) hello) "]

counting up and down:

> m5 = m4 ++ macros [
>       " succ = (lambda (n) (lambda (f) (lambda (x) (f ((n f) x))))) ",
>--       " pred = (lambda (n) (((n (lambda (p) (lambda (z) ((z (succ (p true))) (p true))))) (lambda (z) ((z 0) 0))) false)) "]
>       " pred = (lambda (n) (lambda (f) (lambda (x) (((n (lambda (g) (lambda (h) (h (g f))))) (lambda (u) x)) (lambda (u) u))))) "]

> e5 = [" ((repeat (succ 2)) hello) ",
>       " ((repeat (pred 2)) hello) "]

arithmetic:

> m6 = m5 ++ macros [
>       " sum = (lambda (m) (lambda (n) (lambda (f) (lambda (x) ((m f) ((n f) x)))))) ",
>       " product = (lambda (m) (lambda (n) (lambda (f) (m (n f))))) "]

> e6 = [" ((repeat ((sum 2) 3)) hello) ",
>       " ((repeat ((product 2) 3)) hello) "]

predicate:

> m7 = m6 ++ macros [
>       " isZero = (lambda (n) ((n (true false)) true)) "]

> e7 = [" (((ifThenElse (isZero 0)) this) that) ",
>       " (((ifThenElse (isZero 1)) this) that) "]
  
recursion:

> m8 = m7 ++ macros [
>       " Y = (lambda (y) ((lambda (x) (y (x x))) (lambda (x) (y (x x))))) ",
>       " nfact = (lambda (f) (lambda (n) (((ifThenElse (isZero n)) 1) ((product n) (f (pred n)))))) ",
>       " factorial = (Y nfact) "]

> e8 = [" ((repeat (factorial ((product 2) 2))) hello) "]
