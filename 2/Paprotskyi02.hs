{-# OPTIONS_GHC -Wall #-}
module Paprotskyi02 where

-- Task 1 -----------------------------------------
sumFl :: [Integer] -> Integer
sumFl xs = foldl (+) 0 xs
  
-- Task 2 ----------------------------------------- 
productFr :: [Integer] -> Integer
productFr xs = foldr (*) 1 xs

-- Task 3 -----------------------------------------
concatFr :: [Int] -> [Int] -> [Int]
concatFr xs ys = foldr (:) ys xs

-- Task 4 -----------------------------------------
insert :: [Int] -> Int -> [Int]
insert [] v = [v]
insert (x:xs) v
    | x < v = x : insert xs v
    | otherwise = v : insert xs x

sortInsert :: [Int] -> [Int]
sortInsert [] = []
sortInsert ys = foldl insert [] ys

-- Task 5 -----------------------------------------
findIndices ::(Int -> Bool) -> [Int] -> [Int] 
findIndices p xs = [snd x | x <- (zipWith (\x y -> (x,y)) xs [0..]), p (fst x)]

-- Task 6 -----------------------------------------
allReverse :: [String] -> [String]
allReverse xss = map reverse (reverse xss)

-- Task 7  -----------------------------------------
noDigits :: String -> String
noDigits xs = filter (\x -> not (elem x "1234567890")) xs

-- Task 8 ------------------------------------------
cntGood :: [Int -> Bool] -> Int -> Int
cntGood ps x = length [p | p <- ps, p x]

-- Task 9 ------------------------------------------
trianglePas :: [[Integer]]
trianglePas =  iterate (\x -> zipWith (+) ([0] ++ x) (x ++ [0])) [1]

-- Task 10 -----------------------------------------
factorialWith0Factorial :: [Integer]
factorialWith0Factorial = 1 : zipWith (*) factorialWith0Factorial [1..]
--deleting first 1 just to match the task ("take  5 factorialsM  = [1, 2, 6, 24, 120]")
factorialsM :: [Integer]
factorialsM = tail (factorialWith0Factorial)