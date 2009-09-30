Author: Joseph Pecoraro
Date: Wednesday September 30, 2009
Description: Function Programming Assignment #3
Haskell

> module Matrix where


Helper Functions
----------------

> split :: (a -> a -> b) -> [a] -> b
> split f [x,y] = f x y

> row :: [a] -> Int -> a
> row = (!!)

> col :: [[a]] -> Int -> [a]
> col matrix i = [ row !! i | row <- matrix ]
> col' m i = map (!!i) m

@ split (+) [1,2] == 3
@ split (-) [2,1] == 1
@ row [[1,2],[3,4]] 0 == [1,2]
@ row [[1,2],[3,4]] 1 == [3,4]
@ col [[1,2],[3,4]] 0 == [1,3]
@ col [[1,2],[3,4]] 1 == [2,4]
@ col' [[1,2],[3,4]] 0 == [1,3]
@ col' [[1,2],[3,4]] 1 == [2,4]


(c) Matrix Addition
-------------------

The generic solution applies a function on pairs of elements in the matrices.
Both matrix addition and subtraction can be handled by writing a generic
solution and implanting the proper function.

Precondition: Both matrices must have the same dimension

> matrixop :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
> matrixop _ [] [] = []
> matrixop f (a:as) (b:bs) = zipWith f a b : matrixop f as bs

> add = matrixop (+)
> sub = matrixop (-)

@ add [[1,2],[3,4]] [[5,6],[7,8]] == [[6,8],[10,12]]


(d) Inner Product
-----------------

Pair combines two elements into a list. Trivial!

> pair :: a -> a -> [a]
> pair a b = [a,b]

The other functions are nearly identical to the built-in zipWith function.

> inner = zipWith
> mplus = zipWith (+)

@ pair 1 2 == [1,2]
@ pair [1] [2] == [[1],[2]]
@ inner pair [1,2,3] [4,5,6] == [[1,4],[2,5],[3,6]]
@ inner (+) [1,2,3] [4,5,6] == [5,7,9]
@ inner mplus [[1,2],[3,4]] [[5,6],[7,8]] == [[6,8],[10,12]]


(e) Transpose
-------------

A Matrix's rows and columns are swapped. Our implementation simply
reads the columns of the provided matrix and outputs them as rows.
The columns are read in order from the first to the last.

> transpose :: [[a]] -> [[a]]
> transpose [] = []
> transpose matrix = [ col matrix i | i <- [0..lengthOfRow - 1] ]
>     where lengthOfRow = length (head matrix)

> transpose' :: [[a]] -> [[a]]
> transpose' [] = []
> transpose' matrix@(r:_) = map (col matrix) [0..total]
>     where total = length r - 1

@ transpose  [[1,2,3],[4,5,6]] == [[1,4],[2,5],[3,6]]
@ transpose  [[1,4],[2,5],[3,6]] == [[1,2,3],[4,5,6]]
@ transpose' [[1,2,3],[4,5,6]] == [[1,4],[2,5],[3,6]]
@ transpose' [[1,4],[2,5],[3,6]] == [[1,2,3],[4,5,6]]



(f) Cartesian Product
---------------------

A provided function (f) is run on every pairing of elements between the first
and second lists. Ordering is such that the first element of the first list is
applied with the first element of the second list, the second element of the
second list, and so on until the second list is exhausted. These calculations
produce one sublist of output. This process continues until the first list is
exhausted. The output is one list containing all the resulting sublists.

> cross :: (a -> b -> c) -> [a] -> [b] -> [[c]]
> cross f m1 m2  = [ [ f x y | y <- m2 ] | x <- m1 ]

> cross' :: (a -> b -> c) -> [a] -> [b] -> [[c]]
> cross' f m1 m2  = map (\a -> zipWith f (repeat a) m2) m1

@ cross  pair [1,2,3] [4,5,6] == [[[1,4],[1,5],[1,6]],[[2,4],[2,5],[2,6]],[[3,4],[3,5],[3,6]]]
@ cross  pair [[1,2],[3,4]] [[5,6],[7,8]] == [[[[1,2],[5,6]],[[1,2],[7,8]]],[[[3,4],[5,6]],[[3,4],[7,8]]]]
@ cross' pair [1,2,3] [4,5,6] == [[[1,4],[1,5],[1,6]],[[2,4],[2,5],[2,6]],[[3,4],[3,5],[3,6]]]
@ cross' pair [[1,2],[3,4]] [[5,6],[7,8]] == [[[[1,2],[5,6]],[[1,2],[7,8]]],[[[3,4],[5,6]],[[3,4],[7,8]]]]


(g) Matrix Multiplication
-------------------------

In Matrix Multiplication: m1 x m2
 - [transpose] the second matrix is transposed
 - [cross pair] cross pairs are created (each row from m1 is paired with each from m2)
 - [multiplyLists] multiplication is applied between those two lists
 - [sumInnards] the results of the multiplications are summed

Below is the example of `mul [[1,2],[3,4]] [[5,6],[7,8]]`:

                         [                      [                  [               
                          [                      [                  [          
 ---------   ---------     [[1,2],[5,7]],         [[5,14],           [19],     -----------
 | 1 | 2 |   | 5 | 6 |     [[1,2],[6,8]]   -->     [6,16]]    -->    [22]      | 19 | 22 |
 |---|---| x |---|---|    ],               -->   ],           -->   ],      =  |----|----|
 | 3 | 4 |   | 7 | 8 |    [                -->   [            -->   [          | 43 | 50 |
 ---------   ---------     [[3,4],[5,7]],         [[15,28],          [43],     -----------
                           [[3,4],[6,8]]           [18,32]]          [50]   
                          ]                      ]                  ]            
                         ]                      ]                  ]             

Each step was broken down into easy to manage chunks:

> -- Lists
> mul :: (Num a) => [[a]] -> [[a]] -> [[a]]
> mul m1 m2 = [ map (sum . multiplyLists) x | x <- cross pair m1 (transpose m2) ]
>     where multiplyLists list = split (inner (*)) list

> -- Pipeline
> mul' :: (Num a) => [[a]] -> [[a]] -> [[a]]
> mul' m1 m2 = map (map (sum . multiplyLists)) $ cross pair m1 (transpose m2)
>     where multiplyLists list = split (inner (*)) list

@ mul  [[1,2],[3,4]] [[5,6],[7,8]] == [[19,22],[43,50]]
@ mul  [[1,2,3]] [[4],[5],[6]] == [[32]]
@ mul  [[4],[5],[6]] [[1,2,3]] == [[4,8,12],[5,10,15],[6,12,18]]
@ mul' [[1,2],[3,4]] [[5,6],[7,8]] == [[19,22],[43,50]]
@ mul' [[1,2,3]] [[4],[5],[6]] == [[32]]
@ mul' [[4],[5],[6]] [[1,2,3]] == [[4,8,12],[5,10,15],[6,12,18]]
