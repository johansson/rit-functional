Author: Joseph Pecoraro
Date: Tuesday October 6, 2009
Description: Function Programming Assignment #4
Haskell

> module LambdaCalculus where


Data Types for Lambda Procedures.  We only have to deal with 3 things:

  1. a                # variable   (Name)
  2. (lambda (x) x)   # procedure  (Proc)
  3. (x y)            # invocation (Call)


> data Lambda = Name [Char]
>             | Proc [Char] Lambda
>             | Call Lambda Lambda
>             deriving (Show, Eq)


For Example:
------------

  (((lambda (x) (lambda (x) x)) a) b)

Is:

  (
    (
      (lambda (x)
        (lambda (x) x)
      )
    a)
  b)

Which Is:

  Call
    Call
      Proc "x"
        Proc "x" (Name x)
      -
    Name "a"
  Name "b"
  
Which condenses to:

  Call (Call (Proc "x" (Proc "x" (Name "x"))) (Name "a")) (Name "b")


Church Booleans:
----------------

  true =  (lambda (x) (lambda (y) x))
  false = (lambda (x) (lambda (y) y))
  if-then-else = (lambda (cond) (lambda (then) (lambda (else) ((cond then) else))))

> lambda_true    = Proc "x" (Proc "y" (Name "x"))
> lambda_false   = Proc "x" (Proc "y" (Name "y"))
> lambda_if_else = Proc "cond"
>                   (Proc "then"
>                     (Proc "else"
>                       (Call
>                         (Call (Name "cond") (Name "then"))
>                         (Name "else"))))
