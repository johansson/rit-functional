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