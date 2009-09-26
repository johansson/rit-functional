Author: Joseph Pecoraro
Date: Friday September 25, 2009
Description: Function Programming Assignment #2
Haskell

> module Select where
> import qualified Subset


(b) Sublists
------------

This is the Reverse of Fixed Sublists.  Selecting a subset of size N is the
same as producing all the sublists by removing (length list - N) elements.
Thus this is implemented using our generic solution above for part (a).

> select :: (Eq b) => Int -> [b] -> [[b]]
> select n list = Subset.subn (length list - n) list

@ xsort (select 1 [1,2,3])   == xsort [[3],[2],[1]]
@ xsort (select 2 [1,2,3])   == xsort [[2,3],[1,3],[1,2]]
@ xsort (select 4 [1,2,3])   == xsort []
@ xsort (select 4 [1,2,3,4]) == xsort [[1,2,3,4]]
@ xsort (select 0 [1,2,3,4]) == xsort [[]]
@ xsort (select 2 [1,2,3,4]) == xsort [[3,4],[2,4],[2,3],[1,4],[1,3],[1,2]]
