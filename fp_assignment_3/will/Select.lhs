Select by Will Johansson. Basically this is a combination function, nothing else.

>	module Select where
>		select :: Int -> [a] -> [[a]]
>		select 0 _ = [[]]
>		select _ [] = []
>		select n (x:xs) = map (x:) (select (n-1) xs) ++ select n xs