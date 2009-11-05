// Author: Joseph Pecoraro (jjp1820)
// Date: Thursday November 5, 2009
// Description: Functional Programming Assignment #9
// JavaScript


// --------------------------------
//   Queens Puzzle Implementation
// --------------------------------

function QueensPuzzle(dim, rows, ups, downs) {
    this.dim = dim;
    this.rows = rows;
    this.ups = ups;
    this.downs = downs;
}

QueensPuzzle.prototype = {
    solved: function() {
        return (this.dim === this.rows.length);
    },

    choices: function() {
        var keys = {};
        for (var i=0; i<this.dim; ++i)
            if (this.safe(i))
                keys[i] = i;
        return keys;
    },

    choose: function(row) {
        var col = this.rows.length + 1;
        var newRows = this.rows.slice(0);
        var newUps = this.ups.slice(0);
        var newDowns = this.downs.slice(0);
        newRows.push(row);
        newUps.push(row-col);
        newDowns.push(row+col);
        return new QueensPuzzle(this.dim, newRows, newUps, newDowns);
    },

    safe: function(row) {
        var col = this.rows.length + 1;
        return (this.rows.indexOf(row) === -1 &&
                this.ups.indexOf(row-col) === -1 &&
                this.downs.indexOf(row+col) === -1);
    },

    toString: function() {
        return this.dim + " dimensions\n" + this.rows.toString();
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

var puzzle = new QueensPuzzle(8, [], [], []);
var result = solve(puzzle);
print(result.toString());
