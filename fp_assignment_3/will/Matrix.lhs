Matrix implemented by Will, some different techniques than Joe.

>	module Matrix where

add
	Matrix addition

>		add [] [] = []

Note, very important to have the parentheses around the + operator!
This snagged me.

Two versions, because after doing inner, it's obvious it can be
implemented this way.

Glorified for add' left right = inner mplus left right

>		add (x:xs) (y:ys) = zipWith (+) x y : add xs ys
>		add' left right = zipWith (zipWith (+)) left right
		
pair a b
	returns a list with a, b
	why is this too easy? :)

>		pair a b = [a,b]

inner function

>		inner = zipWith

mplus function

>		mplus = zipWith (+)

Transpose

Basically, here we have a loop over each element in xs, which is
a row, and use subscripts to get content of the elements and put them in a new
list and return that.

>		transpose :: [[a]] -> [[a]]
>		transpose [] = []
>		transpose xs = [[row !! i | row <- xs] | i <- [0..length (head xs)-1]]

Cross, aka cartesian product

So we just loop through the first list and second list, apply the function
func to elements obtained from the loops.

	CAREFUL! This has a bug, can you find it?
		cross func a b = [[func x y | x <- a, y <- b]]

>		cross func a b = [[func x y | x <- a] | y <- b]

Matrix multiplcation

Just basically do in order: transpose arg1, cross pair, do an inner multiply and sum them. Great place to do lambdas.

Grr, I want to convert this to a lambda. It's a bit confusing.
(map func list), replacing func with lambda, so we'd be replacing split
(\f [y,z] -> f y z) is our lambda for split,
but how the heck do we pass the arguments to it?
Works for summing because y is obtained from the second map, which is the multiplication of the contents.

>		split func [a,b] = func a b
>		mul first second = [map (\y -> foldr (+) 0 y) (map (split (inner (*))) x) | x <- cross pair first (transpose second)]

>		mul' first second = [map (foldr (+) 0) (map (split (inner (*))) x) | x <- cross pair first (transpose second)]