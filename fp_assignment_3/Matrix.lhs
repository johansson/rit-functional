Author: Joseph Pecoraro
Date: Friday September 25, 2009
Description: Function Programming Assignment #2
Haskell

> module Matrix where


Helper Functions
----------------

> split :: (a -> a -> b) -> [a] -> b
> split f [x,y] = f x y

> row :: [a] -> Int -> a
> row matrix i = matrix !! i

> col :: [[a]] -> Int -> [a]
> col matrix i = [ row !! i | row <- matrix ]


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


(d) Inner Product
-----------------

Pair combines two elements into a list. Trivial!

> pair :: a -> a -> [a]
> pair a b = [a,b]

The other functions are nearly identical to the built-in zipWith function.

> inner = zipWith
> mplus = zipWith (+)


(e) Transpose
-------------

A Matrix's rows and columns are swapped. Our implementation simply
reads the columns of the provided matrix and outputs them as rows.
The columns are read in order from the first to the last.

> transpose :: [[a]] -> [[a]]
> transpose [] = []
> transpose matrix = [ col matrix i | i <- [0..lengthOfRow - 1] ]
>     where lengthOfRow = length (head matrix)


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
 ---------   ---------     [[1,2],[5,7]],         [[5,14]],          [19],     -----------
 | 1 | 2 |   | 5 | 6 |     [[1,2],[6,8]]   -->    [[6,16]]    -->    [22]      | 19 | 22 |
 |---|---| x |---|---|    ],               -->   ],           -->   ],      =  |----|----|
 | 3 | 4 |   | 7 | 8 |    [                -->   [            -->   [          | 43 | 50 |
 ---------   ---------     [[3,4],[5,7]],         [[15,28]],         [43],     -----------
                           [[3,4],[6,8]]          [[18,32]]          [50]   
                          ]                      ]                  ]            
                         ]                      ]                  ]             

Each step was broken down into easy to manage chunks:

> mul :: (Num a) => [[a]] -> [[a]] -> [[a]]
> mul m1 m2 = [ map sumInnards $ map multiplyLists x | x <- cross pair m1 (transpose m2) ]
>     where multiplyLists list = split (inner (*)) list
>           sumInnards    list = foldr (+) 0 list
