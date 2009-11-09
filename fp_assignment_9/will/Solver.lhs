Will Johansson
Haskell Solver

> module Solver where

Finally got a solve function that works with list comprehension!

> solve puzzle = case solve' puzzle of
>  [] -> []
>  result -> head result
>
> solve' puzzle = case solved puzzle of
>   True -> [puzzle]
>   False -> [y | a <- (choices puzzle), y <- solve' (choose puzzle a), solved y]