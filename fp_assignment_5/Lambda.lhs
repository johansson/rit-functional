Lambda Calculus Interpreter

Will Johansson, Joseph Pecoraro

Definition stolen from Professor's solution for Week 4.

> module Lambda where
> data (Eq a, Show a) => SExpr a =
>   {- variable -}         Name a |
>   {- procedure -}        Proc a (SExpr a) |
>   {- invocation -}       Call (SExpr a) (SExpr a) |
>   {- macro -}            Macro a (SExpr a)
>   deriving (Eq, Show)
> type Macros = [(String, SExpr String)]

Substitute a macro.

We'll need to go through the list (list comprehension?)
to match whatever that's in SExpr, but SExpr is possibly
recursive, for example:

> true = Macro "true" (Proc "x" (Proc "y" (Name "x")))
> false = Macro "false" (Proc "x" (Proc "y" (Name "y")))
> ifThenElse = Macro "if-then-else" (Proc "cond" (Proc "then" (Proc "else" (Call (Call (Name "cond") (Name "then")) (Name "else")))))

booleanThis = Call (Call (Call (Name "if-then-else") (Name "true")) (Name "this")) (Name "that")

Clearly we'll need to substitute (Name "if-then-else").

booleanThis = Call (Call (Call (Proc "cond" (Proc "then" (Proc "else" (Call (Call (Name "cond") (Name "then")) (Name "else"))))))) /* FIX ME, I forgot this / that, just fix it later. You get the point */

> substitute :: Macros -> SExpr String -> SExpr String 
> substitute macros (Proc a x) = Proc a (substitute macros x)
> substitute macros (Call x y) = Call (substitute macros x) (substitute macros y)
> substitute macros (Name x)   = if length filtered == 1 then (snd.head) filtered else Name x
>   where filtered = filter ((==x).fst) macros

> macros = [ ("id", (Proc "x" (Name "x"))),
>            ("true", (Proc "x" (Proc "y" (Name "x")))),
>            ("false", (Proc "x" (Proc "y" (Name "y")))),
>            ("if-then-else", (Proc "cond" (Proc "then" (Proc "else" (Call (Call (Name "cond") (Name "then")) (Name "else")))))),
>            ("0", (Proc "f" (Proc "x" (Name "x")))),
>            ("1", (Proc "f" (Proc "x" (Call (Name "f") (Name "x"))))),
>            ("2", (Proc "f" (Proc "x" (Call (Name "f") (Call (Name "f") (Name "x")))))),
>            ("3", (Proc "f" (Proc "x" (Call (Name "f") (Call (Name "f") (Call (Name "f") (Name "x"))))))) ];

> expr = (Call (Call (Name "true") (Name "x")) (Name "y"));



Do beta reductions.

> nicebeta :: SExpr String -> SExpr String
> nicebeta x = beta $ substitute macros x

> beta :: SExpr String -> SExpr String
> beta (Name a) = Name a
> beta (Proc a x) = Proc a (beta x)
> beta (Call (Name a) x) = Call (Name a) (beta x)
> beta (Call (Proc a x) y) = beta $ substitute [(a, y)] x
> beta (Call x y) = beta $ Call (beta x) (beta y)
> beta x = x


> -- beta $ (Call (substitute macros (Name "id")) (Name "hello"))
> -- nicebeta $ (Call (Name "id") (Name "hello"))

-- id
((lambda (x) x) hello)

-- id expanded
( (lambda (x) (lambda (y) x) y) )

-- 2
( (2 (lambda (f) (f hello))) (lambda (x) x))

-- 1
(Call (Call (Name "1") (Proc "ff" (Call (Name "ff") (Name "hello")))) (Proc "xx" (Name "xx")))

-- the example
(Call (Call (Proc "x" (Proc "y" (Name "x"))) (Name "y")) (Name "foo"))

