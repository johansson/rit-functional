Will Johansson's version
Pretty much a straight port from JavaScript.
s/getIndicesForPosition/getPositionsToCheck/

I _love_ how the actual SLOC for the puzzle model is only 21 lines of code. :)

> module Sudoku where
> import Data.List
> import Maybe
>
> solved matrix = 0 `notElem` matrix
>
> choices matrix = [x | x <- [1..9], x `notElem` (map (matrix!!) (getIndicesForPosition pos))] where
>   pos = fromJust (findIndex (==0) matrix)
>
> getIndicesForPosition pos = nub(rowvalues ++ colvalues ++ squarevalues) where
>   rowvalues = [x | x <- [0..80], x >= (pos `div` 9)*9, x < (pos `div` 9 + 1)*9, x /= pos]
>   colvalues = [x | x <- [0..80], x `mod` 9 == pos `mod` 9, x /= pos]
>   squarevalues = [y+x*9 | x <- take 3 [startRow..], y <- take 3 [startCol..], y+x*9 /= pos]
>   startRow = case ((pos `div` 9) `mod` 3) of
>     0 -> pos `div` 9
>     1 -> (pos `div` 9) - 1
>     2 -> (pos `div` 9) - 2
>   startCol = case (pos `mod` 3) of
>     0 -> pos `mod` 9
>     1 -> (pos `mod` 9) - 1
>     2 -> (pos `mod` 9) - 2
>
> choose matrix move = beforemove ++ [move] ++ aftermove where
>   (beforemove,wherezerowas:aftermove) = splitAt pos matrix
>   pos = fromJust (findIndex (==0) matrix)

solve' seems to only work for Sudoku puzzles. I will try later to figure
out how to make it work for N queens as well. Does not terminate
when it finds a y, it just keeps going on. I guess the reason
why it works for Sudoku because there is exactly one solution. But
there are multiple solutions to N queens, which is why solve' would
return if solving the N queens.

> solve' puzzle = case solved puzzle of
>   True -> puzzle
>   False -> [y | a <- (choices puzzle), y <- solve' (choose puzzle a)]

For the time being, use Joe's solve.

> solve puzzle = case solved puzzle of
>   True  -> puzzle
>   False -> solveChoices (choices puzzle)
>   where
>     solveChoices [] = []
>     solveChoices (x:xs) = case solve (choose puzzle x) of
>       [] -> solveChoices xs
>       result -> result