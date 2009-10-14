Author: Joseph Pecoraro
Date: Wednesday October 14, 2009
Description: Function Programming Assignment #5
Haskell

> module Lambdas where


SExpr Data Type and Macro Type
------------------------------

> data (Eq a, Show a) => SExpr a =
>   Name a |
>   Proc a (SExpr a) |
>   Call (SExpr a) (SExpr a)
>   deriving (Eq, Show)

> type Macros = [(String, SExpr String)]


Substitute Function
-------------------

Take a name and replace all occurances of that name in a given SExpr with an
associated SExpr for the original name (a Macro).

> substitute :: Macros -> SExpr String -> SExpr String 
> substitute macros (Proc a x) = Proc a (substitute macros x)
> substitute macros (Call x y) = Call (substitute macros x) (substitute macros y)
> substitute macros (Name x)   = if length filtered == 1 then (snd.head) filtered else Name x
>   where filtered = filter ((==x).fst) macros


Beta Reductions (First Attempt)
-------------------------------

Input and Output is:

  Name -> Name
  Proc -> Proc with beta reduction
  Call of Proc -> Substitute the arg into the Proc
  Call of Call which reduces to a Proc -> beta reduce Call Proc
  Call otherwise -> beta reduction of arguments

> beta :: SExpr String -> SExpr String
> beta (Name a) = Name a
> beta (Proc a x) = Proc a (beta x)
> beta (Call (Proc a x) y) = beta $ substitute [(a, y)] x
> beta (Call (Call x y) z) = case beta (Call x y) of
>   (Proc a b) -> beta $ Call (Proc a b) z
>   other      -> Call other (beta z)
> beta (Call x y) = Call x (beta y)

@ beta (Call (Proc "x" (Name "x")) (Name "hello")) == (Name "hello")


Beta Reductions (Second Attempt)
--------------------------------

The above is too greedy with its variable substitutions. So I will have the
of an unbound variable (Left String) and a bound variable (Right SExpr String).
I will encode and decode SExpr String equations into and out of this new type,
which I call a BoundSExpr:


> type BoundSExpr = SExpr (Either String (SExpr String))

> encode :: SExpr String -> BoundSExpr
> encode (Name a)   = Name (Left a)
> encode (Proc a x) = Proc (Left a) (encode x)
> encode (Call x y) = Call (encode x) (encode y)

> decode :: BoundSExpr -> SExpr String
> decode (Name (Left a))   = Name a
> decode (Name (Right x))  = x
> decode (Proc (Left a) x) = Proc a (decode x)
> decode (Call x y)        = Call (decode x) (decode y)


The act of binding is to go as deep into the tree as you can, changing existing
unbound variables to bound variables. The scoping is lexical, so if you're in
the act of binding (Left a) then stop if you meet a new Proc with (Left a).

> bind :: Either String (SExpr String) -> SExpr String -> BoundSExpr -> BoundSExpr
> bind (Left a) x (Name (Left b))   | a == b = Name (Right x)
> bind (Left a) x (Proc (Left b) y) | a == b = Proc (Left b) y
> bind (Left a) x (Proc (Left b) y) | a /= b = Proc (Left b) (bind (Left a) x y)
> bind a x (Call y z) = Call (bind a x y) (bind a x z)
> bind _ _ x = x


Now the real beta function. It encodes, runs the BoundSExpr beta reducer, then
decodes into the result.

> beta' :: SExpr String -> SExpr String
> beta' sexpr = decode $ beta'' $ encode sexpr

> beta'' :: BoundSExpr -> BoundSExpr
> beta'' (Name (Left a))   = (Name (Left a))
> beta'' (Name (Right a))  = (Name (Right a))
> beta'' (Proc (Left a) x) = Proc (Left a) (beta'' x)
> beta'' (Call (Name (Right b)) x)                 = beta'' $ Call (encode b) x          -- this is a problem
> beta'' (Call (Proc (Left a) x) (Name (Right b))) = beta'' $ bind (Left a) b x
> beta'' (Call (Proc (Left a) x) y)                = beta'' $ bind (Left a) (decode y) x -- this is a problem
> beta'' (Call (Call x y) z) = case beta'' (Call x y) of
>   (Proc a b) -> beta'' $ Call (Proc a b) z
>   other      -> Call other (beta'' z)
> beta'' (Call x y) = Call x (beta'' y)


Built-in Macros
---------------

> macros = [ ("id",           Proc "x" (Name "x")),
>            ("true",         Proc "x" (Proc "y" (Name "x"))),
>            ("false",        Proc "x" (Proc "y" (Name "y"))),
>            ("if-then-else", Proc "cond" (Proc "then" (Proc "else" (Call (Call (Name "cond") (Name "then")) (Name "else"))))),
>            ("0",            Proc "f" (Proc "x" (Name "x"))),
>            ("1",            Proc "f" (Proc "x" (Call (Name "f") (Name "x")))),
>            ("2",            Proc "f" (Proc "x" (Call (Name "f") (Call (Name "f") (Name "x"))))),
>            ("3",            Proc "f" (Proc "x" (Call (Name "f") (Call (Name "f") (Call (Name "f") (Name "x")))))),
>            ("succ",         Proc "n" (Proc "f" (Proc "x" (Call (Name "f") (Call (Call (Name "n") (Name "f")) (Name "x")))))),
>            ("add",          Proc "m" (Proc "n" (Proc "f" (Proc "x" (Call (Call (Name "m") (Name "f")) (Call (Call (Name "n") (Name "f")) (Name "x"))))))),
>            ("repeat",       Proc "n" (Proc "x" (Call (Call (Name "n") (Proc "g" (Call (Name "g") (Name "x")))) (Proc "y" (Name "y")))))
>          ];


Helpers
-------

> sub :: SExpr String -> SExpr String
> sub x = substitute macros x

> nicebeta :: SExpr String -> SExpr String
> nicebeta x = beta $ sub x

> nicebeta' :: SExpr String -> SExpr String
> nicebeta' x = beta' $ sub x


Tests
-----

> _two = substitute macros $ (Call (Call (Name "2") (Proc "ff" (Call (Name "ff") (Name "hello")))) (Proc "xx" (Name "xx")))
> _notfoo = Call (Call (Proc "x" (Proc "y" (Name "x"))) (Name "y")) (Name "foo")
> _id = Call (Name "id") (Name "id")

@ nicebeta' _two == Call (Name "hello") (Name "hello")
@ nicebeta' _notfoo == Name "y"
@ nicebeta' _id == substitute macros (Name "id")
