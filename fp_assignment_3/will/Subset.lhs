Different version by Will Johansson, using Select.lhs

>	module Select where
>		select :: Int -> [a] -> [[a]]
>		select 0 _ = [[]]
>		select _ [] = []
>		select n (x:xs) = map (x:) (select (n-1) xs) ++ select n xs

Using select from Select.lhs

>		sub :: Int -> [a] -> [[a]]
>		sub n xs = select (length xs - n) xs
>		sub1 xs = sub 1 xs
>		sub2 xs = sub 2 xs
>		sub3 xs = sub 3 xs

@ sub1 [1,2,3,4] =~= [[1,2,3],[1,2,4],[1,3,4],[2,3,4]]
@ sub2 [1,2,3,4] =~= [[1,2],[1,3],[1,4],[2,3],[2,4],[3,4]]
@ sub3 [1,2,3,4] =~= [[1],[2],[3],[4]]
@ sub1 "PROGRAM" =~= ["PROGRA","PROGRM","PROGAM","PRORAM","PRGRAM","POGRAM","ROGRAM"]
