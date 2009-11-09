Author: Joseph Pecoraro (jjp1820)
Date: Thursday November 5, 2009
Description: Functional Programming Assignment #9
Haskell

> module Sudoku where

Represent a sudoku puzzle as a single list of 81 positions,
where positions 0..8 are row 1, 9..17 are row 2, etc. The
values in the positions are 0 for not-set, and 1-9 for set.


Solved
------

A board is solved when all values are set (because we could
not set a value unless it was legal).

> solved board = 0 `notElem` board


Choices
-------

Find the first non-set position (==0) and find all possible moves
for that position.  Moves are of the form:

    (position, possibleValue)

To determine possible values you must look at the position's context.
The context includes the Row, Column, and Shape that position is in.
The possible values are the values from 1-9 that do not occur in other
positions in the context.

> choices board = choicesForPosition p (p `div` 9) (p `mod` 9) where
>     p = firstWhere (==0) board
>
>     choicesForPosition (-1) _ _ = []
>     choicesForPosition position row col = openValues where
>         theRow   = take 9 [(row*9)..]
>         theCol   = take 9 [col,(col+9)..]
>         theShape = take 9 $ genShape ((theLowRow*9)+theLowCol)
>         theLowRow = [0,0,0,3,3,3,6,6,6] !! row
>         theLowCol = [0,0,0,3,3,3,6,6,6] !! col
>         genShape start = start:(start+1):(start+2):(genShape (start+9))
>         theContext = theRow ++ theCol ++ theShape
>         takenValues = map (board!!) theContext
>         openValues = map (\x->(position,x)) $ filter (`notElem` takenValues) [1..9]


Make a Choice
-------------

Set a particular position. Clever use of (_:back) to ignore the
current value in the split position and replace it with value.

> choose board (n, value) = front ++ [value] ++ back where
>   (front, _:back) = splitAt n board


Helpers
-------

Find the position of the first element in a list such that a
predicate is true. I used recursion instead of dropWhile or the
like because they are not graceful.

> firstWhere pred list = inner 0 list where
>   inner _ [] = -1
>   inner p (x:xs) = case pred x of
>     True  -> p
>     False -> inner (p+1) xs


== Copied From Solve.lhs ==

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


Sample Data
-----------

From Page-A-Day Sudoku Calendar, April-19-2008
Sample Board from => http://www.cs.rit.edu/~ats/fp-2009-1/9/Problems.lhs


> sudoku =     [ 0, 4, 6,  0, 0, 0,  8, 9, 0,
>                0, 7, 0,  4, 0, 9,  0, 1, 0,
>                5, 0, 0,  0, 8, 0,  0, 0, 6,
>              
>                0, 0, 3,  9, 0, 8,  6, 0, 0,
>                9, 0, 0,  0, 0, 0,  0, 0, 2,
>                0, 0, 8,  5, 0, 2,  1, 0, 0,
>              
>                4, 0, 0,  0, 5, 0,  0, 0, 3,
>                0, 2, 0,  1, 0, 6,  0, 7, 0,
>                0, 9, 7,  0, 0, 0,  5, 2, 0 ]

> easy =       [ 2, 0, 0,  0, 0, 1,  0, 3, 8,
>                0, 0, 0,  0, 0, 0,  0, 0, 5,
>                0, 7, 0,  0, 0, 6,  0, 0, 0,
>
>                0, 0, 0,  0, 0, 0,  0, 1, 3,
>                0, 9, 8,  1, 0, 0,  2, 5, 7,
>                3, 1, 0,  0, 0, 0,  8, 0, 0,
>
>                9, 0, 0,  8, 0, 0,  0, 2, 0,
>                0, 5, 0,  0, 6, 9,  7, 8, 4,
>                4, 0, 0,  2, 5, 0,  0, 0, 0 ]

> gentle =     [ 0, 1, 0,  4, 2, 0,  0, 0, 5,
>                0, 0, 2,  0, 7, 1,  0, 3, 9,
>                0, 0, 0,  0, 0, 0,  0, 4, 0,
>
>                2, 0, 7,  1, 0, 0,  0, 0, 6,
>                0, 0, 0,  0, 4, 0,  0, 0, 0,
>                6, 0, 0,  0, 0, 7,  4, 0, 3,
>
>                0, 7, 0,  0, 0, 0,  0, 0, 0,
>                1, 2, 0,  7, 3, 0,  5, 0, 0,
>                3, 0, 0,  0, 8, 2,  0, 7, 0 ]

> diabolical = [ 0, 9, 0,  7, 0, 0,  8, 6, 0,
>                0, 3, 1,  0, 0, 5,  0, 2, 0,
>                8, 0, 6,  0, 0, 0,  0, 0, 0,
>
>                0, 0, 7,  0, 5, 0,  0, 0, 6,
>                0, 0, 0,  3, 0, 7,  0, 0, 0,
>                5, 0, 0,  0, 1, 0,  7, 0, 0,
>
>                0, 0, 0,  0, 0, 0,  1, 0, 9,
>                0, 2, 0,  6, 0, 0,  3, 5, 0,
>                0, 5, 4,  0, 0, 8,  0, 7, 0 ]

> unsolvable = [ 1, 0, 0,  9, 0, 7,  0, 0, 3,
>                0, 8, 0,  0, 0, 0,  0, 7, 0,
>                0, 0, 9,  0, 0, 0,  6, 0, 0,
>
>                0, 0, 7,  2, 0, 9,  4, 0, 0,
>                4, 1, 0,  0, 0, 0,  0, 9, 5,
>                0, 0, 8,  5, 0, 4,  3, 0, 0,
>
>                0, 0, 3,  0, 0, 0,  7, 0, 0,
>                0, 5, 0,  0, 0, 0,  0, 4, 0,
>                2, 0, 0,  8, 0, 6,  0, 0, 9 ]

> minimal =    [ 0, 9, 8,  0, 0, 0,  0, 0, 0,
>                0, 0, 0,  0, 7, 0,  0, 0, 0,
>                0, 0, 0,  0, 1, 5,  0, 0, 0,
>                  
>                1, 0, 0,  0, 0, 0,  0, 0, 0,
>                0, 0, 0,  2, 0, 0,  0, 0, 9,
>                0, 0, 0,  9, 0, 6,  0, 8, 2,
>
>                0, 0, 0,  0, 0, 0,  0, 3, 0,
>                5, 0, 1,  0, 0, 0,  0, 0, 0,
>                0, 0, 0,  4, 0, 0,  0, 2, 0 ]