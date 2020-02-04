{-# OPTIONS_GHC -Wall #-}
module Paprotskyi06 where

import Data.List(nub, sort)

type GraphS = (Int,[(Int,Int)])
type Graph  = [[Int]]

-- 1 ------------------------------------
isOrdinary :: Graph -> Bool
isOrdinary [] = error "Empty list was passed as an argument"
isOrdinary gr = checkTwoDirConn gr && checkNoLoops gr && checkNotMultigraph gr

--checks whether two vertex are connected in both directions
checkTwoDirConn :: Graph -> Bool
checkTwoDirConn gr = null [i | (i, x) <- zip [0..] gr, y <- x, notElem i (gr !! y)]

--checks whether a graph doesn't contain loops
checkNoLoops :: Graph -> Bool
checkNoLoops gr = null [i | (i, x) <- zip [0..] gr, elem i x]

--checks whether a graph is not a multigraph
checkNotMultigraph :: Graph -> Bool
checkNotMultigraph []     = True
checkNotMultigraph (g:gr) = checkNoDuplicates g && checkNotMultigraph gr

checkNoDuplicates :: (Ord a) => [a] -> Bool
checkNoDuplicates xs = length (nub xs) == length xs

-- 2 ------------------------------------
fromGraph :: Graph -> GraphS
fromGraph gr = (length gr - 1, [(ind, y) | (ind, xs) <- zip [0..] gr, y <- xs])

-- 3 ------------------------------------
toGraph :: GraphS -> Graph
toGraph grs = [[s | (v, s) <- snd grs, v == x] | x <- [0..(fst grs)]] 

-- 4 ------------------------------------
allWays :: Graph -> Int -> [[[Int]]]
allWays gr v = until condW stepW [[[v]]]
    where condW :: [[[Int]]] -> Bool
          condW = null . head
          stepW :: [[[Int]]] -> [[[Int]]]
          stepW wss = [t:(p:ps) | (p:ps) <- head wss, notElem p ps, t <- gr !! p] : wss

--all ways from a to b in graph gr
waysFromTo :: Graph -> Int -> Int -> [[Int]]
waysFromTo gr a b = [reverse (x:xs)| l <- (allWays gr a), (x:xs) <- l, x == b]

shortWay :: Graph -> Int -> Int -> [Int]
shortWay a b = shortest . waysFromTo a b

--shortest list from list of lists
shortest :: [[Int]] -> [Int]
shortest [] = []
shortest xss = snd $ minimum $ [(length xs, xs) | xs <- xss]
--[ 
--  [ [] ],                                                          --5
--  [ [1,2,1,3,0] ],                                                 --4
--  [ [1,2,1,0],[0,3,1,0],[1,3,1,0],[0,1,3,0],[2,1,3,0],[3,1,3,0] ], --3
--  [ [0,1,0],[2,1,0],[3,1,0],[0,3,0],[1,3,0] ],                     --2
--  [ [1,0],[3,0] ],                                                 --1
--  [ [0] ]                                                          --0
--]
--
--[ [ [0] ] ]                                                            --step0
--[ [[1,0], [3,0]] , [[0]] ]                                             --step1
--[ [[0,1,0],[2,1,0],[3,1,0],[0,3,0],[1,3,0]] , [[1,0], [3,0]] , [[0]] ] --step2

-- 5 ------------------------------------
goNodes :: Graph -> Int -> [Int]
goNodes gr v = snd $ until condN (oneStep gr) ([v],[])

condN :: ([Int],[Int]) -> Bool
condN (ns, _) = null ns

oneStep :: Graph -> ([Int],[Int]) -> ([Int],[Int])
oneStep gr (ns, os) = 
      let   old = ns ++ os
            ns1 = ns >>= (gr!!)
            ns2 = filter (`notElem` old) ns1
            new = nub ns2
      in    (new,old)

isConnecting :: Graph -> Bool
isConnecting gr = length (goNodes gr 0) == length gr

-- 6 ------------------------------------
-- all vertex of Graph g
nodes :: Graph -> [Int]
nodes g = [0..(length g - 1)]

components :: Graph -> [[Int]] 
components gr = nub [sort $ goNodes gr x | x <- nodes gr]

-- 7 ------------------------------------
eccentricity :: Graph -> Int -> Int
eccentricity gr v = maximum [length $ shortWay gr v x | x <- nodes gr] - 1

-- 8 ------------------------------------
allEccentricities :: Graph -> [Int]
allEccentricities gr = [eccentricity gr x | x <- nodes gr]

findDiameter :: Graph -> Int 
findDiameter = maximum . allEccentricities

findRadius :: Graph -> Int 
findRadius = minimum . allEccentricities

-- 9 ------------------------------------
allEccentricitiesWithNodes :: Graph -> [(Int, Int)]
allEccentricitiesWithNodes gr = [(eccentricity gr x, x) | x <- nodes gr]

findCenter :: Graph -> [Int] 
findCenter gr =
    let r = findRadius gr
    in  [n | (e,n) <- allEccentricitiesWithNodes gr, e == r]
-- 10 ------------------------------------
shortWays :: Graph -> Int -> Int -> [[Int]] 
shortWays gr a b =
    let wft = waysFromTo gr a b
        slength = shortestLength wft
    in  filter (\x -> length x == slength) wft

--returns length of the shortest list in a list of lists
shortestLength :: [[Int]] -> Int
shortestLength [] = 0
shortestLength xss = fst $ minimum $ [(length xs, xs) | xs <- xss]
----------------------------
gr1S, gr2S:: GraphS
gr1S = (5,[(0,1),(0,2),(0,3),(1,0),(1,3),(1,4),
           (2,0),(2,4),(2,5),(3,0),(3,1),(4,1),(4,2),(5,2)])
gr2S = (7,[(0,1),(0,3),(1,0),(1,2),(2,1),(2,3),(3,0),(3,2),
           (4,5),(4,6),(5,4),(5,6), (6,4),(6,5)])

gr1, gr2:: Graph
gr1 = [[1,2,3],[0,3,4],[0,4,5],[0,1],[1,2],[2]]
gr2 = [[1,3],[0,2],[1,3],[0,2],[5,6],[4,6],[4,5],[]]