Author: Joseph Pecoraro (jjp1820)
Date: Thursday November 5, 2009
Description: Functional Programming Assignment #9
Haskell


> module Solve where

Solve Function
--------------

> solve puzzle = case solved puzzle of
>   True  -> puzzle
>   False -> solveChoices (choices puzzle)
>   where
>     solveChoices [] = []
>     solveChoices (x:xs) = case solve (choose puzzle x) of
>       [] -> solveChoices xs
>       result -> result
