// Author: Joseph Pecoraro (jjp1820)
// Date: Thursday November 5, 2009
// Description: Functional Programming Assignment #9
// JavaScript


// --------------------------------
//   Sudoku Puzzle Implementation
// --------------------------------

function SudokuPuzzle(board, empty) {
    this.board = board;
}

SudokuPuzzle.prototype = {
    solved: function() {
        return (this.board.indexOf(0) === -1);
    },

    choices: function() {
        var keys = {}, keyCount = 0;
        for (var i=0, len=this.board.length; i<len; ++i) {
            if (this.board[i] === 0) { // find the first open position
                var choicesForPosition = this.choicesForPosition(i);
                for (var j=0; j<choicesForPosition.length; ++j)
                    keys[keyCount++] = choicesForPosition[j];
                return keys;
            }
        }

        return keys; // no moves at this point
    },

    choose: function(choice) {
        var newBoard = this.board.slice(0);
        newBoard[choice.position] = choice.value;
        return new SudokuPuzzle(newBoard);
    },

    choicesForPosition: function(position) {
        var row = Math.floor(position/9);
        var col = position%9;
        var used = [false, false, false, false, false,
                    false, false, false, false, false];

        for (var i=0, j=row*9; i<9; ++i, ++j)
            used[ this.board[j] ] = true;

        for (var i=0, j=col; i<9; ++i, j+=9)
            used[ this.board[j] ] = true;

        var shape = this.shape(row, col);
        for (var i=0; i<9; ++i)
            used[ this.board[shape[i]] ] = true;

        var results = [];
        for (var i=1; i<=9; ++i) // ignore used[0] because we don't care about 0
            if (!used[i])
                results.push({ position: position, value: i });
        return results;
    },

    shape: function(row, col) {
        var indexer = [0,0,0,3,3,3,6,6,6];
        var lowRow = indexer[row];
        var lowCol = indexer[col];
        var start = (lowRow*9) + lowCol;
        return [start   , start+1 , start+2,
                start+9 , start+10, start+11,
                start+18, start+19, start+20];

        // Or Dynamically generate the shape with:
        // for (var i=0, start=(lowRow*9)+lowCol; i<3; ++i) {
        //     shape = shape.concat( [start, start+1, start+2] );
        //     start += 9;
        // }
    },

    toString: function() {
        // NOTE: string manipulation algorithm is known to be inefficient
        var str = 'Board:\n------\n';
        var lines = 0;
        var board = this.board;
        for (var i=0, len=board.length; i<len; ++i) {
            str += board[i] + '  ';
            if ((i+1)%9 === 0) // new line (double between every 3rd line)
                str += (++lines%3 === 0 && lines !== 9) ? "\n\n" : "\n";
            else if ((i+1)%3 === 0) // extra space
                str += ' ';
        }
        return str;
    }
}


// --------------------
//   Array Extensions
// --------------------

// indexOf
// Simplification of https://developer.mozilla.org/en/Core_JavaScript_1.5_Reference/Objects/Array/indexOf
// Almost always implemented natively, but just to be safe
if (!Array.prototype.indexOf) {
    Array.prototype.indexOf = function(elem, from) {
        var len = this.length
        for (var i = (from||0); i<len; ++i)
            if (this[i] === elem)
                return i;
        return -1;
    }
}


// ----------
//   Driver
// ----------

// From Page-A-Day Sudoku Calendar, April-19-2008
// Sample Board from => http://www.cs.rit.edu/~ats/fp-2009-1/9/Problems.lhs
var trivialBoard = [ 0, 4, 6,  0, 0, 0,  8, 9, 0,
                     0, 7, 0,  4, 0, 9,  0, 1, 0,
                     5, 0, 0,  0, 8, 0,  0, 0, 6,

                     0, 0, 3,  9, 0, 8,  6, 0, 0,
                     9, 0, 0,  0, 0, 0,  0, 0, 2,
                     0, 0, 8,  5, 0, 2,  1, 0, 0,

                     4, 0, 0,  0, 5, 0,  0, 0, 3,
                     0, 2, 0,  1, 0, 6,  0, 7, 0,
                     0, 9, 7,  0, 0, 0,  5, 2, 0 ];

var puzzle = new SudokuPuzzle(trivialBoard);
var result = solve(puzzle);
print(result.toString());
