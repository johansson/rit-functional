Select by Will Johansson. Basically this is a combination function, nothing else.

>	module Select where
>		select :: Int -> [a] -> [[a]]
>		select 0 _ = [[]]
>		select _ [] = []
>		select n (x:xs) = map (x:) (select (n-1) xs) ++ select n xs

@ select 1 [1,2,3]   =~= [[1],[2],[3]]
@ select 2 [1,2,3]   =~= [[1,2],[1,3],[2,3]]
@ select 4 [1,2,3]   =~= []
@ select 4 [1,2,3,4] =~= [[1,2,3,4]]
@ select 0 [1,2,3,4] =~= [[]]
@ select 2 [1,2,3,4] =~= [[1,2],[1,3],[1,4],[2,3],[2,4],[3,4]]
