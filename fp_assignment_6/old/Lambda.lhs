Author: Axel Schreiner
Date: Monday October 19, 2009
Description: Function Programming Assignment #5 Solution
This module is designed to be replaced by location in a particular directory.

> module Lambda where
> import Symbol
> import LambdaTypes


Each of the following functions can be applied to a string containing
an s-expression in Scheme notation to show the successive steps.
Note that 'macros' have to be 'Macros Symbol', e.g., ([]::Macros Symbol).

> -- r = read :: String -> SExpr Symbol
> -- rs :: Macros Symbol -> String -> SExpr Symbol
> -- rs macros = substitute macros . r
> -- rsw macros = wrap . rs macros
> -- rswa macros = alpha [] 0 . rsw macros
> -- rswab macros = uncurry beta . rswa macros
> -- rswab' macros = (\(x, _, _) -> x) . rswab macros
> -- rswab'u macros = unwrap . rswab' macros
> -- rswab'us macros = show . rswab'u macros



substitute macros s-expression
------------------------------

Eeturns an s-expression with names replaced by macro values.
This will re-substitute and loop on a self-referential substitution,
i.e., it is not suitable for a beta reduction.

- substitute :: (Eq a, Show a) => Macros a -> SExpr a -> SExpr a

> substitute macros expr = s expr where
>   s var@(Name name)  = case filter ((name ==).fst) macros of
>                          [(macro,expr)] -> {- s -} expr
>                          [] -> var
>   s (Proc parm body) = Proc parm (s body)
>   s (Call proc arg)  = Call (s proc) (s arg)


wrap s-expression
-----------------

Return the expression with each name and parameter marked with 0.

- wrap :: (Eq t, Show t, Num t1) => SExpr t -> SExpr (t1, t)

> wrap (Name name)      = Name (0, name)
> wrap (Proc parm body) = Proc (0, parm) (wrap body)
> wrap (Call proc arg)  = Call (wrap proc) (wrap arg)


unwrap s-expression
-------------------

Return the expression with each markup removed.

- unwrap   :: (Eq t, Eq t1, Show t, Show t1) => SExpr (t, t1) -> SExpr t1

> unwrap (Name (_, name))      = Name name
> unwrap (Proc (_, parm) body) = Proc parm (unwrap body)
> unwrap (Call proc arg)       = Call (unwrap proc) (unwrap arg)


alpha bindings count s-expression
---------------------------------

Replaces each parameter by the same name with a new, unique count,
returns the s-expression and the new count. Each parameter and its
new value is pushed onto the bindings and the count is always
incremented.

- alpha :: (Num a, Eq t, Show t) =>
             [((a, t), (a, t))] -> a -> SExpr (a, t) -> (a, SExpr (a, t))

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
-----------------------

Substitutes the argument for a parameter in a procedure call, returns the
s-expression, the new count, and True if a substitution happened;
the top level returns False. Recurses into call but not into argument,
recurses after substitution.

- beta :: (Num t, Eq t1, Show t1) =>
            t -> SExpr (t, t1) -> (SExpr (t, t1), t, Bool)

> beta count  var@(Name _)        = (var,  count,  False)
> beta count proc@(Proc _ _)      = (proc, count,  False)
> beta count      (Call proc arg) = if true then beta c e else result where
>   result@(e, c, true) = case beta count proc of
>     (Proc parm body, count1, _) -> (expr, count2, True) where
>       (count2, expr) = sub count1 body parm arg
>     (expr, count1, tf)          -> (Call expr arg, count1, tf)


sub count s-expression parm arg
-------------------------------

Substitutes an alpha-converted arg for each parm in the s-expression,
returns the s-expression and the new count.

- sub :: (Num a, Eq t, Show t) =>
           a -> SExpr (a, t) -> (a, t) -> SExpr (a, t) -> (SExpr (a, t), a)

> sub count expr parm arg = s count expr where
>   s count var@(Name name)
>     | name == parm  = alpha [] count arg
>     | otherwise     = (count, var)

>   s count (Call proc arg)  = (count2, Call proc1 arg1) where
>     (count1, proc1) = s count proc
>     (count2, arg1)  = s count1 arg
  
>   s count (Proc name body) = (count1, Proc name body1) where
>     (count1, body1) = s count body
