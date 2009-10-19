Author: Joseph Pecoraro
Date: Monday October 19, 2009
Description: Function Programming Assignment #6
Haskell

> module Symbol where


Symbol Data Type
----------------

> data Symbol =
>   Symbol String
>   deriving (Eq)

> instance Show (Symbol) where
>   show (Symbol x) = x

> instance Read (Symbol) where
>   readsPrec _ s = [(Symbol a, x) | (a, x) <- lex s]