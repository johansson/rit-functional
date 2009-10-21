This module contains some examples.

> module Lambda where
> import LambdaTypes

lexical scoping:
   (((lambda (x) (lambda (x) x)) a) b)   # returns b
   
  beta scope True False -- requires NO closure
  beta' scope
  
> scope  = Call (Call (Proc "x" (Proc "x" (Name "x"))) (Name "a")) (Name "b")

Church booleans:
  $ true =  (lambda (x) (lambda (y) x))
  $ false = (lambda (x) (lambda (y) y))
  $ if-then-else = (lambda (cond) (lambda (then) (lambda (else) ((cond then) else))))

  (((if-then-else true)  this) that)
  (((if-then-else false) this) that)

  beta (substitute booleans this) False True -- requires closure
  beta (substitute booleans that) True True
  beta' (substitute booleans this)
  beta' (substitute booleans that)

> booleans = [
>   ("true",  (Proc "x" (Proc "y" (Name "x")))),
>   ("false", (Proc "x" (Proc "y" (Name "y")))),
>   ("if-then-else", (Proc "cond" (Proc "then" (Proc "else"
>                      (Call (Call (Name "cond") (Name "then")) (Name "else"))))))]

> this = Call (Call (Call (Name "if-then-else") (Name "true"))
>          (Name "this")) (Name "that")
> that = Call (Call (Call (Name "if-then-else") (Name "false"))
>          (Name "this")) (Name "that")

Bug:
  (((lambda (x) (lambda (y) x)) y) foo)
  
  beta bug False False -- produces x or foo, depending on closure
  beta bug False True
  beta' bug

> bug = Call (Call (Proc "x" (Proc "y" (Name "x"))) (Name "y")) (Name "foo")
