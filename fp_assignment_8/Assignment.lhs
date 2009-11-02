Authors: Will Johansson, Professor Schreiner

> module Assignment where

b. The continuation monad can take the place of continuation passing:

> import Monad
> type Next r a = a -> r			-- continuation function
> data Cont r a = Cont ((Next r a) -> r)	-- continuation monad
> instance Monad (Cont r) where
>   return a = Cont (\k -> k a)
>   Cont c >>= f =
>     Cont (\k -> c (\a -> let Cont cc = f a in cc k) )

> Cont c = do x <- return (3+2)
>             return (4*x)

Database stuff for (c)

> staff :: [Person]

> data Person = 
>   Female String String Int | Male String String Int
>   deriving (Eq, Show)

> sFirst (Female first _    _    ) = first
> sFirst (Male   first _    _    ) = first

> sLast  (Female _     last _    ) = last
> sLast  (Male   _     last _    ) = last

> sPhone (Female _     _    phone) = phone
> sPhone (Male   _     _    phone) = phone

> rRoom  = fst
> rPhone = snd

> isMale :: Person -> Bool
> isMale (Female _     _    _    ) = False
> isMale (Male   _     _    _    ) = True

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

fromC staff q1				-- list of all records in staff
fromC staff q2				-- list of male records in staff

Found this on: http://en.wikibooks.org/wiki/Haskell/Advanced_monads

the signature for:

lookup :: Eq a => a  -- a key
       -> [(a, b)]   -- the lookup table to use
       -> Maybe b    -- the result of the lookup

which is close to what we need, jus except the key, the predicate... :\

This compiles, but complains because it couldn't match expected m Bool with inferred Bool,
which makes sense, since isMale is not a -> m Bool, so what to do?

fromC :: (Monad m) => [a] -> (a -> m Bool) -> m [a]

> fromC :: [a] -> (Cont r a) -> [a]
> fromC db cont = db

whichC :: (a -> Bool) -> [a]

> whichC pred = []

> q1 :: t -> Cont u t
> q1 = return

> q2 :: [Person] -> Cont u [Person]
> q2 = whichC isMale