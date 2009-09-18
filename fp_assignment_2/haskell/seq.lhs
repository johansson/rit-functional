Author: Joseph Pecoraro
Date: Thursday, September 17, 2009
Description: Function Programming Assignment #2
Haskell

> module Assignment where

--------------------
1. A sequence of 1's

> repeat' :: a -> [a]
> repeat' n = n : repeat' n

> ones' :: [Integer]
> ones' = [1,1..]

----------------------------------------------------------------------------
2. a function which will return the first n elements of an infinite sequence
s as a list in Haskell and Scheme or an array in JavaScript.

> take' :: (Num i, Ord i, Eq a) => i -> [a] -> [a]
> take' n list
>   | n <= 0     = []
>   | list == [] = []
>   | otherwise  = let (x:xs) = list in x : take' (n-1) xs

-----------------------------------------------------------
3. a sequence of applying f to each element of a sequence s

Alternative:
map' f list = [ f x | x <- list ]

> map' :: (a -> b) -> [a] -> [b]
> map' _ [] = []
> map' f (x:xs) = f x : map' f xs

----------------------------------------------------------------------
4. a sequence of applying f to corresponding elements of two sequences
s1 and s2

> zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
> zipWith' _ [] _ = []
> zipWith' _ _ [] = []
> zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

----------------------------------
5. a sequence of positive integers

> ints' :: [Integer]
> ints' = [1..]

-------------------------------------------
6. a sequence of repeatedly applying f to x
NOTE: the first element in the list is the start value

> iterate' :: (a -> a) -> a -> [a]
> iterate' f n = n : helper f n
>   where helper f n = let m = f n in m : helper f m

---------------------------------------
7. a sequence of positive even integers

> evens' :: [Integer]
> evens' = [2,4..]

--------------------------------------
8. a sequence of positive odd integers

> odds' :: [Integer]
> odds' = [1,3..]

---------------------------------------------
9. a sequence of squares of positive integers

> squares' = map' (^2) ints'

-----------------------------
10. a sequence of powers of n

> powers' n = iterate' (*n) n

----------------------------------------------------
11. a sequence of successive squares starting with n
NOTE: the first element in the resulting list is the start value

> square' = iterate' (^2)

-------------------------------------------------------
12. a sequence of an element e prefixed to a sequence s

Alternative:
seq' :: a -> [a] -> [a]
seq' e l = e : l

> seq' = (:)

---------------------------------------------------------------------------
13. a sequence of applying f to each two neighboring elements of a sequence
NOTE: mapl only works as long as there are two values to take from, so
the list must be infinite.

> mapl' :: (a -> a -> b) -> [a] -> [b]
> mapl' f (x:y:zs) = f x y : mapl' f (y:zs)

----------------------------------------------
14. a Fibonacci sequence starting with a and b

> fibs' a b = helper a b
>   where helper x y = x : helper y (x+y)

--------------------------------------------------------------------------
15. a sequence of using f cumulatively to combine e with each element of a
sequence s

> -- NOTE: My scanl does not make the first element in the list the accumulator
> scanl' :: (a -> b -> a) -> a -> [b] -> [a]
> scanl' _ _ [] = []
> scanl' f a (x:xs) = let n = f a x in n : scanl' f n xs

> total' = scanl'

----------------------------
16. a sequence of factorials

> facts' :: [Integer]
> facts' = helper 1 1
>   where helper mem n = let curr = mem*n in curr : helper curr (n+1)

---------------
Extra Functions

> head' :: [a] -> a
> head' [] = error "Canno get the head of an empty list"
> head' (x:_) = x

> tail' :: [a] -> [a]
> tail' [] = error "Cannot get the tail of an empty list"
> tail' (_:xs) = xs

> drop' :: (Ord a, Num a) => a -> [b] -> [b]
> drop' n list
>   | n <= 0    = list
>   | otherwise = drop' (n-1) (tail' list)

> foldl' :: (a -> b -> a) -> a -> [b] -> a
> foldl' _ a [] = a
> foldl' f a (x:xs) = let n = f a x in foldl' f n xs


-----------
Extra Notes

Both Fibonacci and Factorial use a common pattern.  They both use a helper
function which takes an accumulator (a memoized value) "a" and some value "b".
This helper function is called recursively producing a list of values in
the usual lazy head/tail approach of "c : recurse d e".

Note that "c", "d", and "e", are values produced only from "a" and "b". Thus
with three functions "f1", "f2", and "f3", each taking in two parameters
"a", and "b" we can simulate the behavior of this memoization list generator.


For instance:

  fibs' x y = helper x y
    where helper a b = a : helper b (a+b)

  FUNC = twistie x y f1 f2 f3
  FUNC = helper x y
    where helper a b = c : helper d  e
                       ^          ^  ^
                 f1(a,b)    f2(a,b)  f3(a,b)

My First Attempt passed a and b in as parameters, thus their types may differ:

-- twistie :: a -> b -> (a -> b -> a) -> (a -> b -> a) -> (a -> b -> b) -> [a]
-- twistie a b f1 f2 f3 = f1 a b : twistie (f2 a b) (f3 a b) f1 f2 f3
-- iiterate f n = n : twistie n f (flip ($)) (flip ($)) second
-- ffibs a b = twistie a b first second (+)
-- ffacts = twistie 1 1 (*) (*) (\_ y -> y + 1)
-- first  x _ = x
-- second _ x = x

This approach created extra functions first and second to get the respective
arguments, but otherwise it worked out well because simple arithmetic
operators work well on two argument pairs.  Then I thought about making
the parameters lists, to avoid making first and second:

-- twistie a b f1 f2 f3 = f1 (a:b:[]) : twistie (f2 (a:b:[])) (f3 (a:b:[])) f1 f2 f3
-- ffibs a b = twistie a b head last folda
-- ffacts = twistie 1 1 foldm foldm (\\(_:y:_) -> y+1)
-- folda = foldl (+) 0
-- foldm = foldl (*) 1

The first and second functions can be replaced with head and last, but now I
need simple functions to add or multiple all the elements of a list togther.
Also, I needed to trick the type inference a little so it would know that b
is a single element and not a list, and this only works when a and b are of the
same type, and thus both fit into a list...


----------
Test Cases

Just Copy and Paste these lines into `ghci` and it should
return "True" for all of them! Cheers.

take' 5 ones'                        == [1,1,1,1,1]
take' 5 (map' (1+) ones')            == [2,2,2,2,2]
take' 5 (zipWith' (+) ones' ones')   == [2,2,2,2,2]
take' 5 ints'                        == [1,2,3,4,5]
take' 5 (iterate' (1+) 1)            == [1,2,3,4,5]
take' 5 evens'                       == [2,4,6,8,10]
take' 5 odds'                        == [1,3,5,7,9]
take' 5 squares'                     == [1,4,9,16,25]
take' 5 (powers' 2)                  == [2,4,8,16,32]
take' 5 (square' 2)                  == [2,4,16,256,65536]
take' 5 (seq' 0 ints')               == [0,1,2,3,4]
take' 5 (mapl' (+) (seq' 0 ints'))   == [1,3,5,7,9]
take' 5 (fibs' 1 2)                  == [1,2,3,5,8]
take' 5 (total' (+) 0 ints')         == [1,3,6,10,15]
take' 5 facts'                       == [1,2,6,24,120]

