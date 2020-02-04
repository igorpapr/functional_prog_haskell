module Graph where

type Graph = [[Int]]
g1 :: Graph
g1 = [[1,2,3], [0,3,4], [0,4,5], [0,1], [1,2], [2]]

-- сусіди вершини v в графі g
adj :: Graph -> Int -> [Int]
adj g v = g !! v

-- всі вершини графа g
nodes :: Graph -> [Int]
nodes g = [0..(length g - 1)]

-- ребро належить графу
edgeIn :: Graph -> (Int, Int) -> Bool
edgeIn g (x,y) = elem y (g!!x)

-- всі ребра графу
edges :: Graph -> [(Int,Int)]
edges g = [(x,y) | x <- nodes g, y <- g!!x]

-- побудувати всі підмножини вершин
subsets :: [Int] -> [[Int]]
subsets [] = [[]]
subsets (x:xs) = let xss = subsets xs
                 in map (x: ) xss ++ xss

-- побудувати всі ребра між вершинами xs
allEdges :: [Int] -> [(Int,Int)]
allEdges xs = [(x,y) | x <-xs, y <-xs, x/=y]

--перевірити що кожне ребро з allEdges xs - ребро графа
 --edgeIn :: Graph -> (Int, Int) -> Bool

isClique :: Graph -> [Int] -> Bool
isClique gr xs = let es = allEdges xs
                 in  null $ filter (not . (edgeIn gr)) es

cliqueNum :: Graph -> Int
cliqueNum gr = let xss = subsets (nodes gr)
                   gs = filter (isClique gr) xss
               in  maximum $ map length gs

goNodes :: Graph -> Int -> [Int]
goNodes gr v = snd $ until cond (oneStep gr) ([v],[])

cond :: ([Int],[Int]) -> Bool
cond (ns, _) = null ns

oneStep :: Graph -> ([Int],[Int]) -> ([Int],[Int])
oneStep gr (ns, os) =
    let old = ns ++ os
        ns1 = concatMap ...
		... -- на фото є 
	in (new, old)