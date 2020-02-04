{-# OPTIONS_GHC -Wall #-}
module Practice9 where


data Branch = Fork Branch Branch | Leaf Int

frigle :: Branch -> [Int]
frigle (Leaf v) = [v]
frigle (Fork l r) = frigle l ++ frigle r

height :: Branch-> Int
height (Leaf _)= 0
height (Fork l r) = 1+ max (height l) (height r) 

minOne:: [Int]->Branch
minOne [x] = Leaf x
minOne xs = let d = div (length xs) 2
                l = minOne (take d xs)
                r = minOne (drop d xs)
            in Fork l r 

minBr :: [Int] -> [Branch]
minBr xs = takeMinBr (brs xs)

brs :: [Int] -> [Branch]
brs [v]= [Leaf v]
brs xs = [Fork lt rt|i<- [1..length xs-1], lt <-brs (take i xs), rt <-brs (drop i xs)]


takeMinBr :: [Branch] -> [Branch]
takeMinBr br= let h = minimum (map height br) --minimum $ map (height) br 
              in filter ((==h). height ) br --filter (\b-> height b == h) br