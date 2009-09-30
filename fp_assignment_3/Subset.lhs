Author: Joseph Pecoraro
Date: Friday September 25, 2009
Description: Function Programming Assignment #2
Haskell

Data.List is included for `nub`.

> module Subset where
> import qualified Data.List


Helper Functions
----------------

> flatten :: [[a]] -> [a]
> flatten [] = []
> flatten (x:xs) = x ++ (flatten xs)

> removeIndex i list
>   | list == [] = []
>   | i == 0     = removeIndex (i-1) (tail list)
>   | otherwise  = let (h:t) = list in h : removeIndex (i-1) t

@ flatten [[1],[2]] == [1,2]
@ flatten [ [[1,2],[2,3]], [[4,5],[6,7]] ] == [[1,2],[2,3],[4,5],[6,7]]
@ removeIndex 0 "ABCCBA" == "BCCBA"
@ removeIndex 4 "ABCCBA" == "ABCCA"


(a) Fixed Sublists
------------------

Generic Implementation followed by curried aliases.

> subn :: (Eq b, Num a) => a -> [b] -> [[b]]
> subn 0 list = [list]
> subn 1 list = [ removeIndex x list | x <- [0..length list - 1] ]
> subn n list = Data.List.nub $ flatten [ subn (n-1) (removeIndex x list) | x <- [0..length list - 1] ]

> sub1 :: (Eq a) => [a] -> [[a]]
> sub1 = subn 1
> sub2 :: (Eq a) => [a] -> [[a]]
> sub2 = subn 2
> sub3 :: (Eq a) => [a] -> [[a]]
> sub3 = subn 3

@ sub1 [1,2,3,4] =~= [[2,3,4],[1,3,4],[1,2,4],[1,2,3]]
@ sub2 [1,2,3,4] =~= [[3,4],[2,4],[2,3],[1,4],[1,3],[1,2]]
@ sub3 [1,2,3,4] =~= [[4],[3],[2],[1]]
@ sub1 "PROGRAM" =~= ["ROGRAM","POGRAM","PRGRAM","PRORAM","PROGAM","PROGRM","PROGRA"]
