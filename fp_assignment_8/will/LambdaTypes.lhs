This module is designed to be replaced by location in a particular directory.

> module LambdaTypes where

The following constructs need to be represented:

> data (Eq a, Show a) => SExpr a =
>   {- variable -}         Name a |
>   {- procedure -}        Proc a (SExpr a) |
>   {- invocation -}       Call (SExpr a) (SExpr a)
>   deriving (Eq, Show)

> type Macros a = [(a, SExpr a)]

where the macro call cannot be distinguished from a variable.
