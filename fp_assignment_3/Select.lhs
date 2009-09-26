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
