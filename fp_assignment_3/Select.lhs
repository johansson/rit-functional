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

@ select 1 [1,2,3]   `xeq` [[3],[2],[1]]
@ select 2 [1,2,3]   `xeq` [[2,3],[1,3],[1,2]]
@ select 4 [1,2,3]   `xeq` []
@ select 4 [1,2,3,4] `xeq` [[1,2,3,4]]
@ select 0 [1,2,3,4] `xeq` [[]]
@ select 2 [1,2,3,4] `xeq` [[3,4],[2,4],[2,3],[1,4],[1,3],[1,2]]
