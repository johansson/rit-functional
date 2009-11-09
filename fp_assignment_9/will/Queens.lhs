N Queens.

Authors: Will Johansson and Prof. Schreiner and Joseph Pecoraro

> module Queens where

represent a queens puzzle as a list of four lists:
  dimension of the puzzle
  for each column the row number of the queen
  up- and down-diagonal numbers of the queens

> queens = [[8::Int], [], [], []]
> solved [[dim], rows, ups, downs] = dim == length rows

represent a choice as a list of as-yet-unused row numbers
which do not clash with diagonals.

> choices [[dim], rows, ups, downs] = [r | r <- [1..dim], safe r]
>   where
>     col = length rows + 1
>     safe row = row `notElem` rows 
>                && (row-col) `notElem` ups
>                && (row+col) `notElem` downs

> choose [[dim], rows, ups, downs] row =
>               [[dim], row:rows, (row-col):ups, (row+col):downs]
>   where
>     col = length rows + 1

function solve (puzzle) {
  if (puzzle.solved()) return puzzle;
  
  var result, choices = puzzle.choices();
  for (var choice in choices)
    if (result = solve(puzzle.choose(choices[choice]))) return result;
  return null;
}

solve' seems to only work for Sudoku puzzles. I will try later to figure
out how to make it work for N queens as well. Does not terminate
when it finds a y, it just keeps going on. I guess the reason
why it works for Sudoku because there is exactly one solution. But
there are multiple solutions to N queens, which is why solve' would
return if solving the N queens.

> solve' puzzle = case solved puzzle of
>  True -> puzzle
>  False -> [y | a <- (choices puzzle), y <- solve' (choose puzzle a)]
  
So use Joe's

> solve puzzle = case solved puzzle of
>   True  -> puzzle
>   False -> solveChoices (choices puzzle)
>   where
>     solveChoices [] = []
>     solveChoices (x:xs) = case solve (choose puzzle x) of
>       [] -> solveChoices xs
>       result -> result