> module Hutton where

from Graham Huttons's web site, http://www.cs.nott.ac.uk/~gmh/sudoku.lhs

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

First gentle example from sudoku.org.uk:

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

First diabolical example:

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

First unsolvable (requires backtracking) example:

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

Minimal sized grid (17 values) with a unique solution:

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