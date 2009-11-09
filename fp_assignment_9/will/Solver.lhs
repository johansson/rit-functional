Will Johansson
Haskell Solver

> module Solver where

solve seems to only work for Sudoku puzzles. I will try later to figure
out how to make it work for N queens as well. Does not terminate
when it finds a y, it just keeps going on. I guess the reason
why it works for Sudoku because there is exactly one solution. But
there are multiple solutions to N queens, which is why solve' would
return if solving the N queens.

> solve puzzle = case solved puzzle of
>   True -> puzzle
>   False -> [y | a <- (choices puzzle), y <- solve (choose puzzle a)]