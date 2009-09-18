-- Haskell
-- Author: Joseph Pecoraro
-- Date: Saturday September 12, 2009
-- Description: Functional Programming Assignment #1

-- Useful Function to Test
perfects = filter (\n -> sum (factors n) == n) [1..]
    where factors n = [i | i <- [1..n `div` 2], n `mod` i == 0]

-- One Implementation
goodtake n list
    | n <= 0     = []
    | list == [] = []
    | otherwise  = let (x:xs) = list in x : take (n-1) xs

-- Alternative Form
goodtake2 :: (Num i) => i -> [a] -> [a]
goodtake2 0 _ = []
goodtake2 _ [] = []
goodtake2 n (x:xs) = x : goodtake2 (n-1) xs