// Sudoku JavaScript solution
// Author: Will Johansson
// Forgive me for the terrible code. :(

function SudokuPuzzle(matrix)
{
	this.matrix = matrix;
	
	this.solved = function ()
	{
		// a solved board with have no zeros, so
		// indexOf should return -1
		return matrix.indexOf(0) == -1;
	};
	
	this.choices = function ()
	{
		// possible choices
		var arr = [1,2,3,4,5,6,7,8,9];
		
		for (var i = 0; i < matrix.length; i++)
		{
			// find first blank position
			if (matrix[i] == 0)
			{
				// indices to get values of whats in
				// those places so we can figure out
				// what to remove from arr.
				positions = this.getPositionsToCheck (i);
				
				for(var j = 0; j < positions.length; j++)
				{
					var a = matrix[positions[j]];
					
					if(a != 0 && arr.indexOf(a) != -1 && arr.length > 0)
					{
						// kill that possible choice
						arr.splice(arr.indexOf(a),1);
					}
				}
				
				return arr;
			}
		}
		
		return [];
	};
	
	// get all indices that we need to check
	this.getPositionsToCheck = function (index)
	{
		var positions = [];
		var pos_i = 0;
		var minInRow = [0,9,18,27,36,45,54,63,72];
		var maxInRow = [8,17,26,35,44,53,62,71,80];

		var positionInRow = index % 9;
		var col_positionIn3x3cell = index % 3;
		
		var row = Math.floor(index / 9); // sigh, not integer division
		var row_positionIn3x3cell = row % 3;
		
		var startCol = positionInRow - col_positionIn3x3cell;
		var startRow = row - row_positionIn3x3cell;
		
		for(var i = minInRow[row]; i <= maxInRow[row]; i++)
		{
			if (i != index)
				positions.push(i);
		}
		
		for(var i = positionInRow; i < matrix.length; i+=9)
		{
			if (i != index)
				positions.push(i);
		}
		
		for(var i = startRow; i < startRow + 3; i++)
		{
			for(var j = startCol; j < startCol + 3; j++)
			{
				var possible = j+i*9;
				
				if (possible != index && positions.indexOf(possible) == -1)
					positions.push(possible);
			}
		
		}
		
		return positions;
		
	};
	
	// we have a choice, make a new puzzle with that choice in
	// the blank position
	this.choose = function (choice)
	{
		var new_matrix = matrix.slice(0);
		var index = 0;
		
		for (var i = 0; i < matrix.length; i++)
		{
			if(matrix[i] == 0)
			{
				index = i;
				break;
			}
		}
		
		new_matrix[index] = choice;
		
		return new SudokuPuzzle(new_matrix);
	};
	
	// make it look fancy. I could have just made
	// this a really fancy HTML table with CSS whizbang
	// but too much to do, and so little time... this works!
	this.toString = function ()
	{
		var retVal = "[\n  ";
		var rows = 0;
		
		for(var i = 0; i < matrix.length; i++)
		{
			retVal += matrix[i];
			if (i != 80) retVal += ", ";
			if (i%3==2) retVal += " ";
			if (i%9==8 && i != 80) { retVal += "\n  "; ++rows; }
			if ((rows-1)%3==2) { if(i!=80) retVal += "\n  "; rows = 0; }
		}
		
		retVal += "\n]";
		
		return retVal;
	};
}
