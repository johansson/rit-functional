Will Johansson
Haskell Solver

> module Solver where

Finally got a solve function that works with list comprehension!

But we have to be careful because head will NOT like [] as an argument, but
I believe this never happens. Tried to circumvent this by:

solve puzzle = case solve' puzzle of
  [] -> []
  result -> head result
  
But the compiler doesn't like it.

> solve puzzle = head $ solve' puzzle
> solve' puzzle = case solved puzzle of
>   True -> [puzzle]
>   False -> [y | a <- (choices puzzle), y <- solve' (choose puzzle a), solved y]