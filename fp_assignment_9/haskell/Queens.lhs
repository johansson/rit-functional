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
