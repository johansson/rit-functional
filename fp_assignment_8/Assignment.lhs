Authors: Joseph Pecoraro and Professor Schreiner
Date: Tuesday November 3, 2009
Description: Function Programming Assignment #8
Haskell

> module Assignment where
> import Monad

Databases (Tables)
------------------

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


Person Record and Helpers
-------------------------

> data Person a b c =
>   Male a b c |
>   Female a b c
>   deriving (Eq, Show)

> sFirst :: Person a b c -> a
> sFirst (Male   a _ _) = a
> sFirst (Female a _ _) = a

> sLast :: Person a b c -> b
> sLast (Male   _ b _) = b
> sLast (Female _ b _) = b

> sPhone :: Person a b c -> c
> sPhone (Male   _ _ c) = c
> sPhone (Female _ _ c) = c

> pFirst = sFirst
> pLast  = sLast
> pPhone = sPhone

> isMale :: Person a b c -> Bool
> isMale (Male _ _ _) = True
> isMale _ = False


Room Helpers
------------

> rRoom  = fst
> rPhone = snd


Cont Monad
----------

> type Next r a = a -> r

> data Cont r a = Cont ((Next r a) -> r)
> instance Monad (Cont r) where
>   return a = Cont (\k -> k a)
>   Cont c >>= f =
>     Cont (\k -> c (\a -> let Cont cc = f a in cc k))



(b) define the type of c and implement f so that it has
the integer value 20 retrieved from c.
-------------------------------------------------------

> c :: (Next Int Int) -> Int
> Cont c = do x <- return (3+2)
>             return (4*x)

> runCont (Cont x) f = f $ x id
> f cont = runCont cont id



(c) determine the types of fromC and whichC, and implement both functions.
--------------------------------------------------------------------------

  fromC staff q1    -- list of all records in staff
  fromC staff q2    -- list of male records in staff

> q1 :: t -> Cont u t
> q1 = return

> q2 :: [Person a b c] -> Cont u [Person a b c]
> q2 = whichC isMale

> fromC :: [a] -> ([a] -> Cont b b) -> b
> fromC table x = f (do x table)

> whichC :: (a -> Bool) -> [a] -> Cont u [a]
> whichC pred table = return (filter pred table)



(d) determine the type of selectC and implement the function
------------------------------------------------------------

  fromC staff q3    -- list of first names in staff
  fromC staff q4    -- list of first names of females

> q3 :: [Person a b c] -> Cont u [a]
> q3 = selectC pFirst

> q4 :: [Person a b c] -> Cont u [a]
> q4 input = do x <- whichC (not.isMale) input
>               q3 x

> selectC :: (a -> b) -> [a] -> Cont u [b]
> selectC func table = return (map func table)



(e) groupC groups records by equal keys and returns a list of tuples consisting
of a key and a list with information drawn from records with the same key
-------------------------------------------------------------------------

@  fromC rooms q5    =~= [(3008,56084),(3005,57146),(3005,52529)]
  		-- list of records in rooms with numbers up to 3008
  	
@  fromC rooms q6    =~= [(3005,[(3005,57146),(3005,52529)]), (3008,[(3008,56084)])]
  		-- list of groups of records
  		
@  fromC rooms q7    =~= [(3005,[57146,52529]), (3008,[56084])]
  		-- list of groups selected from records

> q5       = whichC (\x -> rRoom x <= 3008)
> q6 input = do x <- q5 input
>               groupC rRoom id x
> q7 input = do x <- q5 input
>               groupC rRoom rPhone x

NOTE: This reuses the professor's solution from last week.

> groupC :: Eq u => (t -> u) -> (t -> v) -> [t] -> Cont w [(u,[v])]
> groupC groupFunc applyFunc table = return (group1 groupFunc applyFunc table)

> group1 key report input = g input [] where
>   g [] result     = reverse result
>   g (t:ts) result = g rest ((u, (v:vs)) : result) where
>     u = key t
>     v = report t
>     pairs = zip ts (map key ts)
>     (us, _) = unzip (filter ((u ==).snd) pairs)
>     (rest, _) = unzip (filter ((u /=).snd) pairs)
>     vs = map report us

NOTE: This reuses the my solution from last week.

> groupC' :: Eq u => (t -> u) -> (t -> v) -> [t] -> Cont w [(u,[v])]
> groupC' groupFunc applyFunc table = return (inner [] groupFunc applyFunc table) where
>   inner acc _ _ []     = acc
>   inner acc g f (x:xs) = inner (addToAccumulator acc g f x) g f xs
>   
>   addToAccumulator acc g f x = case groupKey `elem` keys of
>     True  -> applyWhere (\(k,v) -> (k,v++[f x])) ((==groupKey).fst) acc
>     False -> (groupKey, [f x]):acc
>     where
>       groupKey = g x
>       keys = map fst acc
>       applyWhere applyFunc predFunc = map (\a -> if (predFunc a) then applyFunc a else a)



(f) joinC returns a list of tuples which combine an incoming record and a
record from a second list which satisfy a relation on a key from each record
----------------------------------------------------------------------------

@ fromC staff q8 == [(Female "Tina" "Sturgis" 57905,(3012,57905)), (Female "Tina" "Sturgis" 57905,(3671,57905))]

> q8 input = do x <- whichC (\x -> pFirst x == "Tina") input
>               joinC rooms pPhone (==) rPhone x

NOTE: This reuses the professor's solution from last week.

> joinC :: [u] -> (t -> a) -> (a -> b -> Bool) -> (u -> b) -> [t] -> Cont v [(t,u)]
> joinC bs keya pred keyb as = return (join bs keya pred keyb as) where
>   join bs keya predicate keyb as =
>     [(a,b) | a <- as, b <- bs, predicate (keya a) (keyb b)]

NOTE: This reuses the my solution from last week. Also, I switch the params
to a more usable form (left leftFunc <=> rightFunc right).

> joinC' :: [u] -> (t -> a) -> (a -> b -> Bool) -> (u -> b) -> [t] -> Cont v [(t,u)]
> joinC' left rightFunc compFunc leftFunc table = return (inner [] left leftFunc compFunc rightFunc table) where
>   inner acc _ _ _ _ [] = acc
>   inner acc l lf cf rf (x:xs) = inner (singleJoin acc l lf cf rf x) l lf cf rf xs
>   
>   singleJoin acc l lf cf rf x = acc ++ (foldl foldFunc [] l)
>     where 
>       rightResult = rf x
>       foldFunc acc lx = case rightResult `cf` (lf lx) of
>         True  -> acc ++ [(x,lx)]
>         False -> acc



(g) Determine type and result of the following queries
------------------------------------------------------


From a list of People get the list of the first names of all the males:

@  fromC staff k1 == ["Jason","James","Sam"]

> k1 :: [Person a b c] -> Cont u [a]
> k1 input = do x <- whichC isMale input
>               selectC pFirst x


From a list of People get their first name and the room number where
the room's phone number is the same as the person's phone number. Also
the room numbers come from the "rooms" table:

@  fromC staff k2 == [("Sandy",3021),("Tina",3012),("Tina",3671),("Joanne",3008),("Eileen",3005),("Christina",3022),("Liane",3022)]

> k2 :: [Person a b Integer] -> Cont u [(a, Integer)]
> k2 input = do x <- whichC (not.isMale) input
>               y <- joinC rooms pPhone (==) rPhone x
>               selectC (\(p,r) -> (pFirst p, rRoom r)) y


From a list of People run k2. On that output group based on the phone
field (getting a list of phone numbers and the people they belong to).
On that output group based on groups of people. The group forces
an (Eq) constraint on the Person's first name.

@  fromC staff k3 == [(["Sandy"],[3021]),(["Tina"],[3012,3671]),(["Joanne"],[3008]),(["Eileen"],[3005]),(["Christina","Liane"],[3022])]

> k3 :: (Eq a) => [Person a b Integer] -> Cont u [([a], [Integer])]
> k3 input = do x <- k2 input
>               y <- groupC snd fst x
>               groupC snd fst y



(h) orderC sorts list elements according to a relation, determine the
type of orderC and implement the function.
------------------------------------------

@  fromC [4, 1, 3, 6, 7] (orderC (<))    == [1,3,4,6,7]
@  fromC [4, 1, 3, 6, 7] (orderC (>))    == [7,6,4,3,1]

> orderC :: (a -> a -> Bool) -> [a] -> Cont u [a]
> orderC f table = return (qsort f table) where
>   qsort _ [] = []
>   qsort f (h:t) = qsort f [x | x<-t, x `f` h] ++ [h] ++ qsort f [x | x<-t, not $ x `f` h]

