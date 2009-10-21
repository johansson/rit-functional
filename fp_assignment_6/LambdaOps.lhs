Author: Axel Schreiner (unchanged)
Date: Monday October 19, 2009
Description: Function Programming Assignment #5 Solution
This module contains functions which operate on LambdaTypes.

> module LambdaOps where
> import Symbol
> import LambdaTypes

dump s-expression
L() is a newtype wrapper for SExpr to override show and read
for Scheme-like notation. dump unwraps L.

> dump (L expr) = expr

We need to create macros for Scheme-like notation:

> type LMacros a = [(a, LExpr a)]

Each of the following functions can be applied to a string containing
an s-expression in Scheme notation to show the successive steps.
Note that 'macros' have to be '([] :: [(Symbol, LExpr Symbol)])'.

> -- read Scheme-like notation to Haskell-like notation
> r :: String -> SExpr Symbol
> r = dump . read

> -- and substitute macros
> rs :: LMacros Symbol -> String -> SExpr Symbol
> rs macros = substitute macros . r

> -- and get ready for unique names
> rsw :: (Num t) => LMacros Symbol -> String -> SExpr (t, Symbol)
> rsw macros = wrap . rs macros

> -- and make all names unique
> rswa :: (Num a) => LMacros Symbol -> String -> (a, SExpr (a, Symbol))
> rswa macros = alpha [] 0 . rsw macros

> -- and perform all beta reductions
> rswab :: (Num a) => LMacros Symbol -> String -> (SExpr (a, Symbol), a, Bool)
> rswab macros = uncurry beta . rswa macros

> -- and remove state
> rswab' :: (Num t) => LMacros Symbol -> String -> SExpr (t, Symbol)
> rswab' macros = (\(x, _, _) -> x) . rswab macros

> -- and make names ambiguous again
> rswab'u :: LMacros Symbol -> String -> SExpr Symbol
> rswab'u macros = unwrap . rswab' macros

> -- and go back to Scheme-like notation
> rswab'ul :: LMacros Symbol -> String -> LExpr Symbol
> rswab'ul macros = L . rswab'u macros

> -- and convert to strings
> rswab'uls :: LMacros Symbol -> String -> String
> rswab'uls macros = show . rswab'ul macros

substitute macros s-expression
returns an s-expression with names replaced by macro values.

Implemented by structural induction on the s-expression.
This will re-substitute and loop on a self-referential substitution,
i.e., it is not suitable for a beta reduction.

> substitute :: (Eq a, Show a) => LMacros a -> SExpr a -> SExpr a

> substitute macros expr = s expr where
>   s var@(Name name)  = case filter ((name ==).fst) macros of
>                          [(macro, L(expr))] -> s expr
>                          [] -> var
>   s (Proc parm body) = Proc parm (s body)
>   s (Call proc arg)  = Call (s proc) (s arg)

wrap s-expression
return the expression with each name and parameter marked with 0.

Implemented by structural induction on the s-expression.

- wrap :: (Eq t, Show t, Num t1) => LExpr t -> LExpr (t1, t)

> wrap (Name name)      = Name (0, name)
> wrap (Proc parm body) = Proc (0, parm) (wrap body)
> wrap (Call proc arg)  = Call (wrap proc) (wrap arg)

unwrap s-expression
return the expression with each markup removed.

Implemented by structural induction on the s-expression.

- unwrap   :: (Eq t, Eq t1, Show t, Show t1) => LExpr (t, t1) -> LExpr t1

> unwrap (Name (_, name))      = Name name
> unwrap (Proc (_, parm) body) = Proc parm (unwrap body)
> unwrap (Call proc arg)       = Call (unwrap proc) (unwrap arg)

alpha bindings count s-expression
replaces each parameter by the same name with a new, unique count,
returns the s-expression and the new count.

Implemented by structural induction on the s-expression;
each parameter and its new value is pushed onto the bindings
and the count is always incremented.

- alpha :: (Num a, Eq t, Show t) =>
             [((a, t), (a, t))] -> a -> LExpr (a, t) -> (a, LExpr (a, t))

> alpha bound count (Proc parm@(_,p) body) = (newCount, Proc newParm newBody) where
>   newParm             = (count+1, p)
>   (newCount, newBody) = alpha ((parm, newParm) : bound) (count+1) body

> alpha bound count (Name name@(c,n)) = case dropWhile ((name /=).fst) bound of
>   ((parm, newParm) : _) -> (count, Name newParm)
>   []                    -> (count, Name name) -- free

> alpha bound count (Call proc arg) = (newCount, Call newProc newArg) where
>   (nextCount, newProc) = alpha bound count proc
>   (newCount, newArg)   = alpha bound nextCount arg

beta count s-expression
substitutes the argument for a parameter in a procedure call,
returns the s-expression, the new count, and True if a substitution happened;
the top level returns False.

Implemented by structural induction on the s-expression;
recurses into call but not into argument, recurses after substitution.

- beta :: (Num t, Eq t1, Show t1) =>
            t -> LExpr (t, t1) -> (LExpr (t, t1), t, Bool)

> beta count  var@(Name _)        = (var,  count,  False)
> beta count proc@(Proc _ _)      = (proc, count,  False)
> beta count      (Call proc arg) = if true then beta c e else result where
>   result@(e, c, true) = case beta count proc of
>     (Proc parm body, count1, _) -> (expr, count2, True) where
>       (count2, expr) = sub count1 body parm arg
>     (expr, count1, tf)          -> (Call expr arg, count1, tf)

sub count s-expression parm arg
substitutes an alpha-converted arg for each parm in the s-expression,
returns the s-expression and the new count.

Implemented by structural induction on the s-expression.

- sub :: (Num a, Eq t, Show t) =>
           a -> LExpr (a, t) -> (a, t) -> LExpr (a, t) -> (LExpr (a, t), a)

> sub count expr parm arg = s count expr where
>   s count var@(Name name)
>     | name == parm  = alpha [] count arg
>     | otherwise     = (count, var)

>   s count (Call proc arg)  = (count2, Call proc1 arg1) where
>     (count1, proc1) = s count proc
>     (count2, arg1)  = s count1 arg
  
>   s count (Proc name body) = (count1, Proc name body1) where
>     (count1, body1) = s count body