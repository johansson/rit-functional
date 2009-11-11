Will Johansson
Haskell Solver

> module Solver where

Finally got a solve function that works with list comprehension!

> class Puzzle a where
>   solved :: a -> Bool
>   choices :: a -> [b]
>   choose :: a -> b -> a

   solve :: a -> a
   solve puzzle = case solve' puzzle of
     [] -> []
     result -> head result
   solve' :: a -> [a]
   solve' puzzle = case solved puzzle of
     True -> [puzzle]
     False -> [y | a <- (choices puzzle), y <- solve' (choose puzzle a), solved y]

>   solve :: a -> a
>   solve puzzle = case solved puzzle of
>     True  -> puzzle
>     False -> solveChoices (choices puzzle)
>     where
>       solveChoices [] = []
>       solveChoices (x:xs) = case solve (choose puzzle x) of
>         [] -> solveChoices xs
>         result -> result
