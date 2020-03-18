{-# OPTIONS_GHC -Wall #-}
module Paprotskyi01 where

-- Task 1 -----------------------------------------
power3 :: [Integer]
power3 = [x^(3::Integer) | x <- [1 ..]]

-- Task 2 -----------------------------------------
toPower3 :: [Integer]
toPower3 = [3^x | x <-[1 ..]::[Integer]]

-- Task 3 -----------------------------------------
sumPower3 :: Integer -> Integer
sumPower3 n = sum [3^x | x <-[1 .. n]]

-- Task 4 -----------------------------------------
sumPower :: Integer -> Integer -> Integer
sumPower m n = sum [m^x | x <- [1 .. n]]

-- Task 5 -----------------------------------------
numberOfLess :: Int -> [Int] -> Int
numberOfLess _ [] = 0
numberOfLess y ys = if y > (head ys) then 1 + numberOfLess y (tail ys) else numberOfLess y (tail ys)

lessMe :: [Int] -> [Int]
lessMe xs = [v | x <- xs, let v = numberOfLess x xs]

-- Task 6 -----------------------------------------
numberOfEqual :: Int -> [Int] -> Int
numberOfEqual _ [] = 0
numberOfEqual y ys = if y == (head ys) then 1 + numberOfEqual y (tail ys) else numberOfEqual y (tail ys)

filterDup :: (Eq a) => [a] -> [a]
filterDup [] = []
filterDup [x] = [x]
filterDup (x:xs) = x : [ k  | k <- filterDup(xs), k /=x ]

frequency :: [Int] -> [(Int,Int)]
frequency xs = filterDup [(x, v)| x <- xs, let v = numberOfEqual x xs]
-- Task 7 -----------------------------------------
hailstone :: Int -> Int
hailstone n = if (mod n 2) == 0 then div n 2 else n*3 + 1
-- Task 8 -----------------------------------------
hailSeq :: Int -> [Int]
hailSeq n = if n == 1 then [1] else [n]++hailSeq (hailstone n)

-- Task 9 -----------------------------------------
allHailSeq :: [[Int]]
allHailSeq = [v |x <- [1 ..], let v = hailSeq x]

-- Task 10 -----------------------------------------
firstHailSeq :: Int -> Int
firstHailSeq l = fhs allHailSeq l where fhs :: [[Int]] -> Int -> Int
                                        fhs _ 0 = error "There is no empty lists in this sequence"
                                        fhs [] _ = error "List is empty"
                                        fhs (x:xs) p 
                                            |length x == p = head x
                                            |otherwise = fhs xs p 