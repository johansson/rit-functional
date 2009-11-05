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

Find the first non-set position and find all possible moves
for that position.  Moves are of the form:

    (position, possibleValue)

To determine possible values you must look at the position's
context. Which includes the Row, Column, and Shape that
position is in.
 

> choices board = findOpenPosition 0 board
>   where
>     findOpenPosition _ [] = []
>     findOpenPosition p (x:xs) = case x of
>       0 -> choicesForPosition p (p `div` 9) (p `mod` 9)
>       _ -> findOpenPosition (p+1) xs
>
>     choicesForPosition position row col = openValues
>       where
>         theRow   = take 9 [(row*9)..]
>         theCol   = take 9 [col,(col+9)..]
>         theShape = take 9 $ genShape ((theLowRow*9)+theLowCol)
>         theLowRow = [0,0,0,3,3,3,6,6,6] !! row
>         theLowCol = [0,0,0,3,3,3,6,6,6] !! col
>         genShape start = start:(start+1):(start+2):(genShape (start+9))
>         theContext = theRow ++ theCol ++ theShape
>         takenValues = [board !! x | x <- theContext]
>         openValues = [(position, x) | x <- [1..9], x `notElem` takenValues]


Make a Choice
-------------

Set a particular position.

> choose board (n, value) = front ++ [value] ++ (drop 1 back)
>   where (front, back) = splitAt n board


Sample Data
-----------

From Page-A-Day Sudoku Calendar, April-19-2008
Sample Board from => http://www.cs.rit.edu/~ats/fp-2009-1/9/Problems.lhs

> sudoku = [ 0, 4, 6,  0, 0, 0,  8, 9, 0,
>            0, 7, 0,  4, 0, 9,  0, 1, 0,
>            5, 0, 0,  0, 8, 0,  0, 0, 6,
>            
>            0, 0, 3,  9, 0, 8,  6, 0, 0,
>            9, 0, 0,  0, 0, 0,  0, 0, 2,
>            0, 0, 8,  5, 0, 2,  1, 0, 0,
>            
>            4, 0, 0,  0, 5, 0,  0, 0, 3,
>            0, 2, 0,  1, 0, 6,  0, 7, 0,
>            0, 9, 7,  0, 0, 0,  5, 2, 0 ] :: [Int]

