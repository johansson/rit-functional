Author: Joseph Pecoraro
Date: Friday September 25, 2009
Description: Function Programming Assignment #2
Haskell

Data.List is included for `delete`.

> module Subset where
> import Data.List hiding (transpose)


Helper Functions
----------------

> flatten :: [[a]] -> [a]
> flatten [] = []
> flatten (x:xs) = x ++ (flatten xs)


(a) Fixed Sublists
------------------

Generic Implementation followed by curried aliases.

> subn :: (Eq b, Num a) => a -> [b] -> [[b]]
> subn 0 list = [list]
> subn 1 list = [ delete x list | x <- list ]
> subn n list = nub $ flatten [ subn (n-1) (delete x list) | x <- list ]

> sub1 :: (Eq a) => [a] -> [[a]]
> sub1 = subn 1
> sub2 :: (Eq a) => [a] -> [[a]]
> sub2 = subn 2
> sub3 :: (Eq a) => [a] -> [[a]]
> sub3 = subn 3
