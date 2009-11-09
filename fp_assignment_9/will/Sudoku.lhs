Will Johansson's version
Pretty much a straight port from JavaScript.
s/getIndicesForPosition/getPositionsToCheck/

I _love_ how the actual SLOC for the puzzle model is only 15 lines of code. :)

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
>   startRow = (pos `div` 9) - ((pos `div` 9) `mod` 3)
>   startCol = (pos `mod` 9) - (pos `mod` 3)
>
> choose matrix move = beforemove ++ [move] ++ aftermove where
>   (beforemove,wherezerowas:aftermove) = splitAt pos matrix
>   pos = fromJust (findIndex (==0) matrix)

Finally got a solve function that works with list comprehension!

But we have to be careful because head will NOT like [] as an argument, but
I believe this never happens. Tried to circumvent this by:

solve puzzle = case solve' puzzle of
  [] -> []
  result -> head result
  
But the compiler doesn't like it.

> solve puzzle = head $ solve' puzzle
> solve' puzzle = case solved puzzle of
>   True -> [puzzle]
>   False -> [y | a <- (choices puzzle), y <- solve' (choose puzzle a), solved y]