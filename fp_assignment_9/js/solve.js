// Puzzle solve (Puzzle puzzle)
//   puzzle.solved()      returns true|false
//   puzzle.choices()     returns a collection of possible moves
//   puzzle.choose(move)  returns the puzzle resulting from the move
// returns first successful state or null.

// The moves returned by choices() must be possible in the current state,
// but there is no guarantee that making one of the moves will produce a state
// which is either solved() or where choices() produces a non-empty collection.

function solve (puzzle) {
  if (puzzle.solved()) return puzzle;

  var result, choices = puzzle.choices();
  for (var choice in choices)
    if (result = solve(puzzle.choose(choices[choice]))) return result;
  return null;
}

// printing in browsers
if (this.window) {
  window.print = function() {
    console.log.apply(console, arguments);
  }
}