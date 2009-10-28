Author: Joseph Pecoraro
Date: Saturday October 24, 2009
Description: Function Programming Assignment #7
Haskell

> module Assignment where


Database
--------

> staff = -- sFirst      sLast        sPhone
>   [ Female "Sandy"     "Ferrara"    55178,
>     Female "Tina"      "Sturgis"    57905,
>     Female "Joanne"    "Catan"      56084,
>     Female "Eileen"    "Wilczak"    57146,
>     Male   "Jason"     "Harrison"   52529,
>     Female "Christina" "Rohr"       52995,
>     Female "Liane"     "Fitzgerald" 52994,
>     Male   "James"     "Craig"      55254,
>     Male   "Sam"       "Waters"     54934 ]

> rooms = -- rRoom rPhone
>   [ (3021, 55178),  -- Sandy
>     (3012, 57905),  -- Tina
>     (3671, 57905),
>     (3008, 56084),  -- Joanne
>     (3005, 57146),  -- Eileen
>     (3005, 52529),  -- Jason
>     (3022, 52995),  -- Christina
>     (3022, 52994),  -- Liane
>     (3599, 55254),  -- James
>     (3596, 54934) ] -- Sam 


(a) Define the type of staff and implement the necessary datatype.
------------------------------------------------------------------

> data Staff a b c =
>   Male a b c |
>   Female a b c
>   deriving (Eq, Show)


(b) Implement access functions sFirst, sLast, sPhone to return first and last
name and phone number from a record of the staff table, and rRoom and rPhone
to return phone and room number from a record of the rooms table as indicated.
------------------------------------------------------------------------------

> sFirst :: Staff a b c -> a
> sFirst (Male   a _ _) = a
> sFirst (Female a _ _) = a

> sLast :: Staff a b c -> b
> sLast (Male   _ b _) = b
> sLast (Female _ b _) = b

> sPhone :: Staff a b c -> c
> sPhone (Male   _ _ c) = c
> sPhone (Female _ _ c) = c

> pFirst = sFirst
> pLast = sLast
> pPhone = sPhone

@ sFirst (Male "first" "last" 12345) == "first"
@ sLast  (Male "first" "last" 12345) == "last"
@ sPhone (Male "first" "last" 12345) == 12345
@ sFirst (Female "first" "last" 12345) == "first"
@ sLast  (Female "first" "last" 12345) == "last"
@ sPhone (Female "first" "last" 12345) == 12345

> rRoom  = fst
> rPhone = snd

@ rRoom  (3021, 55178) == 3021
@ rPhone (3021, 55178) == 55178


(c) Implement a predicate isMale that returns True for a Male, False otherwise.
-------------------------------------------------------------------------------

> isMale :: Staff a b c -> Bool
> isMale (Male _ _ _) = True
> isMale _ = False

@ isMale (Male "Jason" "Harrison" 52529)
@ isMale (Female "Eileen" "Wilczak" 57146) == False


(d) Which function from the Standard Prelude is used as the trivialContinuation
so that `f 2 trivialContinuation` has the indicated value:
----------------------------------------------------------

> f :: a -> (a -> b) -> b
> f n k = k n

> trivialContinuation = id


(e) With a clever definition, arithmetic expressions can be split up into a
cascaded sequence of continuations.
-----------------------------------

> t :: (a -> b) -> (b -> c) -> a -> c
> t a b = b . a

@ (f 2 $ t (3+) $ t (4*) trivialContinuation) == 20


(f) Define general types of from and which and implement both functions.
------------------------------------------------------------------------

  from staff id                 -- list of all records in staff
  from staff $ which isMale id  -- list of male records in staff -- $

> from :: a -> (a -> b) -> b
> from rec func = f rec func

> which :: (a -> Bool) -> ([a] -> b) -> [a] -> b
> which pred cont = t (filter (pred)) cont

@ from staff id == staff
@ (from staff $ which isMale id) == [Male "Jason" "Harrison" 52529, Male "James" "Craig" 55254, Male "Sam" "Waters" 54934] -- $
@ (from staff $ which isMale $ which ((<53000).sPhone) id) == [Male "Jason" "Harrison" 52529]
@ (from staff $ which (not.isMale) $ which ((>57000) . sPhone) id) == [Female "Tina" "Sturgis" 57905,Female "Eileen" "Wilczak" 57146]


(g) Define the general type of select and implement the function.
-----------------------------------------------------------------

  from staff $ select pFirst id                       -- list of first names in staff   -- $
  from staff $ which (not.isMale) $ select pFirst id  -- list of first names of females 

> select :: (a -> b) -> ([b] -> c) -> [a] -> c
> select func cont = t (map func) cont

@ (from staff $ select pFirst id) == ["Sandy","Tina","Joanne","Eileen","Jason","Christina","Liane","James","Sam"] -- $
@ (from staff $ which (not.isMale) $ select pFirst id) == ["Sandy","Tina","Joanne","Eileen","Christina","Liane"]
@ (from staff $ which (isMale) $ select (\(Male a b _) -> a ++ " " ++ b) id) == ["Jason Harrison","James Craig","Sam Waters"] -- \)


(h) group groups records by equal keys and returns a list of tuples consisting
of a key and a list with information drawn from records with the same key.
--------------------------------------------------------------------------

  from rooms $ which (\x -> rRoom x <= 3008) id               -- $
  	-- [(3008,56084),(3005,57146),(3005,52529)]
  	-- list of records in rooms with numbers up to 3008
	
  from rooms $ which (\x -> rRoom x <= 3008) $ group rRoom id id
  	-- [(3005,[(3005,57146),(3005,52529)]), (3008,[(3008,56084)])]
	
  from rooms $ which (\x -> rRoom x <= 3008) $ group rRoom rPhone id
  	-- [(3005,[57146,52529]), (3008,[56084])]

> group :: Eq u => (t -> u) -> (t -> v) -> ([(u,[v])] -> [w]) -> [t] -> [w]
> group groupFunc applyFunc cont = t (inner [] groupFunc applyFunc) cont
>   where
>     inner acc _ _ []     = acc
>     inner acc g f (x:xs) = inner (addToAccumulator acc g f x) g f xs
>   
>     addToAccumulator acc g f x = case groupKey `elem` keys of
>       True  -> applyWhere (\(k,v) -> (k,v++[f x])) ((==groupKey).fst) acc
>       False -> (groupKey, [f x]):acc
>       where
>         groupKey = g x
>         keys = map fst acc
>         applyWhere applyFunc predFunc = map (\a -> if (predFunc a) then applyFunc a else a)

@ (from rooms $ which (\x -> rRoom x <= 3008) id) == [(3008,56084),(3005,57146),(3005,52529)] -- $
@ (from rooms $ which (\x -> rRoom x <= 3008) $ group rRoom id id) == [(3005,[(3005,57146),(3005,52529)]), (3008,[(3008,56084)])]
@ (from rooms $ which (\x -> rRoom x <= 3008) $ group rRoom rPhone id) == [(3005,[57146,52529]), (3008,[56084])]


(i) join returns a list of tuples which combine an incoming record and a record
from a second list which satisfy a relation on a key from each record
---------------------------------------------------------------------

  from staff $ which (\x -> pFirst x == "Tina") $ join rooms pPhone (==) rPhone id
  	-- [(Female "Tina" "Sturgis" 57905,(3012,57905)),
  	--  (Female "Tina" "Sturgis" 57905,(3671,57905))]

Note that I reorder the functions in my "inner" function to what I feel is a
more logical ordering.

> join :: [u] -> (t -> a) -> (a -> b -> Bool) -> (u -> b) -> ([(t,u)] -> [v]) -> [t] -> [v]
> join left rightFunc compFunc leftFunc cont = t (inner [] left leftFunc compFunc rightFunc) cont
>   where
>     inner acc _ _ _ _ [] = acc
>     inner acc l lf cf rf (x:xs) = inner (singleJoin acc l lf cf rf x) l lf cf rf xs
>
>     singleJoin acc l lf cf rf x = acc ++ (foldl foldFunc [] l)
>       where 
>         rightResult = rf x
>         foldFunc acc lx = case rightResult `cf` (lf lx) of
>           True  -> acc ++ [(x,lx)]
>           False -> acc

@ (from staff $ which (\x -> pFirst x == "Tina") $ join rooms pPhone (==) rPhone id) == [(Female "Tina" "Sturgis" 57905,(3012,57905)), (Female "Tina" "Sturgis" 57905,(3671,57905))]


(j) What is the result of the following queries
-----------------------------------------------

> j1 = from   staff
>    $ which  isMale
>    $ select pFirst id
>
> j2 = from   staff
>    $ which  (not.isMale)
>    $ join   rooms pPhone (==) rPhone
>    $ select (\(p,r) -> (pFirst p, rRoom r)) id
>
> j3 = from  j2
>    $ group snd fst
>    $ group snd fst id

j1: ["Jason","James","Sam"]
j2: [("Sandy",3021),("Tina",3671),("Tina",3012),("Joanne",3008),("Eileen",3005),("Christina",3022),("Liane",3022)]
j3: [(["Sandy"],[3021]),(["Tina"],[3012,3671]),(["Joanne"],[3008]),(["Eileen"],[3005]),(["Christina","Liane"],[3022])]

@ j1 == ["Jason","James","Sam"]
@ j2 == [("Sandy",3021),("Tina",3012),("Tina",3671),("Joanne",3008),("Eileen",3005),("Christina",3022),("Liane",3022)]
@ j3 == [(["Sandy"],[3021]),(["Tina"],[3671,3012]),(["Joanne"],[3008]),(["Eileen"],[3005]),(["Christina","Liane"],[3022])]


(k) order sorts list elements according to a relation
-----------------------------------------------------

  order (>) id [4, 1, 3, 6, 7]
  order (<) id [4, 1, 3, 6, 7]

> order :: (a -> a -> Bool) -> ([a] -> b) -> [a] -> b
> order f cont = t (qsort f) cont
>   where
>     qsort _ [] = []
>     qsort f (h:t) = qsort f [x | x<-t, x `f` h] ++ [h] ++ qsort f [x | x<-t, not $ x `f` h]

@ order (>) id [4, 1, 3, 6, 7] == [7,6,4,3,1]
@ order (<) id [4, 1, 3, 6, 7] == [1,3,4,6,7]


(l) Define ascending and descending
-----------------------------------

> l1 = from staff
>    $ which isMale
>    $ order (ascending pPhone (==))
>    $ select pFirst id
>
> l2 = from staff
>    $ which (not.isMale)
>    $ join rooms pPhone (==) rPhone
>    $ select (\(p,r) -> (pFirst p, rRoom r))
>    $ order (ascending fst $ descending snd (==)) id

> ascending :: (Ord b) => (a -> b) -> (a -> a -> Bool) -> a -> a -> Bool
> ascending f cont = \a b -> if (f a) == (f b) then a `cont` b else (f a) < (f b)

> descending :: (Ord b) => (a -> b) -> (a -> a -> Bool) -> a -> a -> Bool
> descending f cont = \a b -> if (f a) == (f b) then a `cont` b else (f a) > (f b) 

@ l1 == ["Jason","Sam","James"]
@ l2 == [("Christina",3022),("Eileen",3005),("Joanne",3008),("Liane",3022),("Sandy",3021),("Tina",3671),("Tina",3012)]
