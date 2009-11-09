// Will Johansson
// FP2009 HW9
function QueensPuzzle(dim, rows, ups, downs)
{
	this.dim = dim;
	this.rows = rows;
	this.ups = ups;
	this.downs = downs;
	
	// > solved [[dim], rows, ups, downs] = dim == length rows
	this.solved = function ()
	{
		return dim == rows.length;
	}
	
	// > choices [[dim], rows, ups, downs] = [r | r <- [1..dim], safe r]
	this.choices = function ()
	{
		var arr = {};
		
		for(i=1;i<=dim;i++)
		{
			if(this.safe(i))
				arr[i]=i;
		}
		
		return arr;
	};
	
	// >     col = length rows + 1
	this.col = function ()
	{
		return this.rows.length + 1;
	}
	
	// >     safe row = row `notElem` rows 
	// >                && (row-col) `notElem` ups
	// >                && (row+col) `notElem` downs
	this.safe = function (row)
	{
		return (this.rows.indexOf(row) == -1 &&
				this.ups.indexOf(row-this.col()) == -1 &&
				this.downs.indexOf(row+this.col()) == -1);
	};
	
	// > choose [[dim], rows, ups, downs] row =
	// >               [[dim], row:rows, (row-col):ups, (row+col):downs]
	this.choose = function (row) {
		var new_rows = this.rows.slice(0);
		var new_ups = this.ups.slice(0);
		var new_downs = this.downs.slice(0);
		
		// Array.push will work as well as Array.unshift,
		// but I'm guessing unshift is more expensive.
		// I don't think order matters? If it does, just
		// loop through the array in reverse or unshift.
		new_rows.push(row);
		new_ups.push(row-this.col());
		new_downs.push(row+this.col());
		
		return new QueensPuzzle(this.dim,new_rows,new_ups,new_downs);
	};
	
	// Get a string representation of the puzzle.
	this.toString = function ()
	{
		var retVal = "[[" + dim + "],[" + rows + "],[" + ups + "],[" + downs + "]]";
		
		return retVal;
	};
}