Author: Joseph Pecoraro
Date: Saturday September 26, 2009
Description: Function Programming Assignment #2
Haskell Testing Suite

> module Tester where

Testing Functions
-----------------

Pass an expression that should be True/False into tst and it will output
"PASS" or "FAIL"!

> tst True  = "PASS"
> tst False = "FAIL"

@ [] == []
@ tst True  == "PASS"
@ tst False == "FAIL"


Sorting (When Order Doesn't Matter)
-----------------------------------

This is a sort useful when the order of the output "DOES NOT MATTER"! It is
implemented as a basic quicksort.

> xsort [] = []
> xsort (h:t) = xsort [x | x <- t, x <= h] ++ [h] ++ xsort [x | x <- t, x > h]
> xeq a b = xsort a == xsort b

@ xsort [9,4,5] == [4,5,9]
@ xsort [[1,2],[2,1]] == xsort [[2,1],[1,2]]
@ [9,4,5] `xeq` [4,5,9]
@ [[1,2],[2,1]] `xeq` [[2,1],[1,2]]
