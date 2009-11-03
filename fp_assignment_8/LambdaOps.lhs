Authors: Joseph Pecoraro and Professor Schreiner
Date: Tuesday November 3, 2009
Description: Function Programming Assignment #8
Haskell

a. Check out the class notes and use the State monad and commented,
literate Haskell to implement a replacement for LambdaOps which has
no explicit count arguments. The replacement must include a set of
driver functions r, etc. and be totally compatible with Lambda.

All sections that have been modified begin with "MODIFIED:" or "NEW".


> module LambdaOps where
> import LambdaLExpr
> import LambdaParse
> import LambdaTypes


NEW: State Monad and Helper Functions
-------------------------------------

> data State a = State (Int -> (a, Int))
> instance Monad State where
>   return x = State (\c -> (x, c))
>   State m >>= f = State (\c ->
>     case m c of { (a, acount) ->
>       case f a of State b -> b acount})

> getState = State (\c -> (c, c))
> putState count = State (\_ -> ((), count))
> increment = do c <- getState
>                putState (c+1)

> getData (State f) = f 0
> getPayload = fst . getData


Driver Functions (The Ones Changed by me are NOTED)
--------------------------------------------------


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


MODIFIED: alpha now returns a State Monad, so rswa must return a State
Monad as well. This decision was so that we could reuse the State Monad
(which encapsulates the count) later in the beta/sub phases which need
that count information.

> -- and make all names unique
> rswa :: LMacros Symbol -> String -> State (SExpr (Int, Symbol))
> rswa macros = alpha [] . rsw macros


MODIFIED: Because rswa/alpha returns a State Monad we need to safely
access the "SExpr (Int, Symbol)" currently inside that monad. This can be
done inside do notation by bridging it across the <-. We can then pass
the SExpr to beta while still inside the Monad to retain the count. beta
itself must return a Monadic value. It keeps true to the original and
returns a Tupled value of (SExpr, Bool), no longer containing the count
as it is in the Monad. That tuple is inside the Monad, which we bring
out again, and return just the SExpr (dropping the Boolean) wrapped in
our State Monad (required).

> -- perform beta reductions
> rswab :: LMacros Symbol -> String -> State (SExpr (Int, Symbol))
> rswab macros = run macros where
>   run macros str = do sexpr <- (rswa macros str)
>                       (x,_) <- beta sexpr
>                       return x


MODIFIED: The old rswab' just removed the count. This rswab' does the
same thing, it removes the count, but it has to do so from a State Monad.
This is done by accessing the payload inside the Monad (defined above)
by running the function inside the State and accessing the Payload portion
of the Monad's internal value:

                   -- what we want
                   |
  State (\x -> (payload, state))
         ^^^^^^^^^^^^^^^^^^^^^
                   |
                   -- a function we need to exec to get it
                      which returns a tuple of which we access
                      the first slot.

> -- remove state
> rswab' :: LMacros Symbol -> String -> SExpr (Int, Symbol)
> rswab' macros = getPayload . rswab macros

> -- and make names ambiguous again
> rswab'u :: LMacros Symbol -> String -> SExpr Symbol
> rswab'u macros = unwrap . rswab' macros

> -- and go back to Scheme-like notation
> rswab'ul :: LMacros Symbol -> String -> LExpr Symbol
> rswab'ul macros = L . rswab'u macros

> -- and convert to strings
> rswab'uls :: LMacros Symbol -> String -> String
> rswab'uls macros = show . rswab'ul macros


Substitute Macros
-----------------

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


Wrap and Unwrap SExpr
---------------------

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


Alpha (Bindings)
----------------

alpha bindings count s-expression
replaces each parameter by the same name with a new, unique count,
returns the s-expression and the new count.

Implemented by structural induction on the s-expression;
each parameter and its new value is pushed onto the bindings
and the count is always incremented.


MODIFIED: alpha handles incrementing the count to give "unique names" to the
variables. The idea here was to mask the counting in a State Monad and carry
that count around inside the Monad instead of as a parameter. alpha is
returning this Monad, and it must do so in all cases (obviously).

The only time the count changes is inside alpha for a Proc. Incrementing the
state is hidden in a helper function. Alpha is called recursively, thus we stay
inside the State Monad and the same count state is manipulated.

alpha used to return an "SExpr (Int, Symbol)" with a shimmyed count with it. We
now return the SExpr wrapped in a State monad which maintains the count! This
is much cleaner.

- alpha
  :: (Eq t, Show t) =>
     [((Int, t), (Int, t))] -> SExpr (Int, t) -> State (SExpr (Int, t))
             
> alpha bound (Proc parm@(_,p) body) =
>   do increment
>      c <- getState -- this is the new state after the increment
>      newBody <- alpha ((parm, (c, p)):bound) body
>      return (Proc (c, p) newBody)

> alpha bound (Name name@(c,n)) = 
>   case dropWhile ((name /=).fst) bound of
>     ((parm, newParm):_) -> return (Name newParm)
>     []                  -> return (Name name)

> alpha bound (Call proc arg) =
>   do newProc <- alpha bound proc
>      newArg  <- alpha bound arg
>      return (Call newProc newArg)

Old Version:
--------------------------------------------------------------------------------
alpha bound count (Proc parm@(_,p) body) = (newCount, Proc newParm newBody) where
  newParm             = (count+1, p)
  (newCount, newBody) = alpha ((parm, newParm) : bound) (count+1) body

alpha bound count (Name name@(c,n)) = case dropWhile ((name /=).fst) bound of
  ((parm, newParm) : _) -> (count, Name newParm)
  []                    -> (count, Name name) -- free

alpha bound count (Call proc arg) = (newCount, Call newProc newArg) where
  (nextCount, newProc) = alpha bound count proc
  (newCount, newArg)   = alpha bound nextCount arg
--------------------------------------------------------------------------------


Beta (Reductions)
-----------------

beta count s-expression
substitutes the argument for a parameter in a procedure call,
returns the s-expression, the new count, and True if a substitution happened;
the top level returns False.


MODIFIED: beta actually performs the reductions. It stays true to the original
implementation/algorithm and passes around a True/False to indicate wether a
substitution happened and needs to happen again or not.

All computation must take place inside of a State Monad do expression. This way
we never have to see the count but it is there in the background. The count is
needed when we run "sub" which may call alpha.

- beta :: (Eq t, Show t) => SExpr (Int, t)
          -> State (SExpr (Int, t), Bool)

> beta   var@(Name _)        = return (var, False)
> beta  proc@(Proc _ _)      = return (proc, False)
> beta       (Call proc arg) =
>  do x <- beta proc
>     case x of
>       (Proc parm body, _) -> do expr <- (sub body parm arg)
>                                 beta expr
>       (expr, tf)          -> if tf then beta expr
>                              else return (Call expr arg, tf)

Old Version:
--------------------------------------------------------------------------
beta count  var@(Name _)        = (var,  count,  False)
beta count proc@(Proc _ _)      = (proc, count,  False)
beta count      (Call proc arg) = if true then beta c e else result where
  result@(e, c, true) = case beta count proc of
    (Proc parm body, count1, _) -> (expr, count2, True) where
      (count2, expr) = sub count1 body parm arg
    (expr, count1, tf)          -> (Call expr arg, count1, tf)
--------------------------------------------------------------------------


Sub (Handle Alpha Substitutions)
--------------------------------

sub count s-expression parm arg
substitutes an alpha-converted arg for each parm in the s-expression,
returns the s-expression and the new count.

Implemented by structural induction on the s-expression.


MODIFIED: Because sub may make a call to alpha, it must have the count
available to it. We pass the count around inside the State Monad, so it follows
that sub must be called in a Monadic context in order to then call alpha in a
Monadic context.

The rest of the implementation is very straight forward. Because sub works
recursively on its inner s function, we can avoid passing around an extra count
by hiding it inside the State Monad. Because we are using the do notation, we
mus return a Monadic value, which is okay to do since "sub" should be used from
inside a do expression and thus the payload value can be brought across the <-.

- sub
    :: (Eq t, Show t) => SExpr (Int, t) -> (Int, t) -> SExpr (Int, t)
       -> State (SExpr (Int, t))

> sub expr parm arg = s expr where
>   s var@(Name name)
>     | name == parm = alpha [] arg
>     | otherwise    = return var
> 
>   s (Call proc arg) =
>     do proc1 <- s proc
>        arg1  <- s arg
>        return (Call proc1 arg1)
> 
>   s (Proc name body) =
>     do body1 <- s body
>        return (Proc name body1)

Old Version:
--------------------------------------------------------------------------
sub count expr parm arg = s count expr where
  s count var@(Name name)
    | name == parm  = alpha [] count arg
    | otherwise     = (count, var)

  s count (Call proc arg)  = (count2, Call proc1 arg1) where
    (count1, proc1) = s count proc
    (count2, arg1)  = s count1 arg

  s count (Proc name body) = (count1, Proc name body1) where
    (count1, body1) = s count body
--------------------------------------------------------------------------
