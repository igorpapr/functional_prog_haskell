{-# OPTIONS_GHC -Wall #-}
module Paprotskyi10 where

import Data.List
import qualified Text.ParserCombinators.Parsec as P
import Data.Maybe(fromMaybe)

data RE = Null   |
          Term Char |
          Seq RE RE |
          Alt RE RE |
          Rep RE    |
          Plus RE   |
          Opt RE
        deriving (Eq, Show)

type State = Int
data Label = C Char | Eps deriving (Eq, Ord, Show)
type Transition = (State, State, Label)
type Automation = (State, [State], [Transition])

type MetaState = [State]
type MetaTransition = (MetaState, MetaState, Label)

-- Задача 1 -----------------------------------------
--re5S = "(ab)?d+"
--re5 = Seq (Opt (Seq (Term 'a') (Term 'b'))) (Plus (Term 'd'))
--"(ab|)dd*"
simplify :: RE -> RE   
simplify Null = Null
simplify (Term c) = Term c
simplify (Seq f s) = Seq (simplify f) (simplify s)
simplify (Alt f s) = Alt (simplify f) (simplify s)
simplify (Rep r) = Rep (simplify r)
simplify (Plus r) = Seq (simplify r) (Rep (simplify r))
simplify (Opt r) = Alt (simplify r) Null

-- Задача 2 -----------------------------------------
isTerminal :: Automation -> State -> Bool 
isTerminal (_, ends, _) s = elem s ends

isEssential :: Automation -> State -> Bool 
isEssential a@(_,_,transs) s
    | isTerminal a s = True
    | otherwise = case find (\x -> findNeededTrans x) transs of
        Nothing -> False
        Just _  -> True
        where findNeededTrans :: Transition -> Bool
              findNeededTrans (st, _, C _) = st == s
              findNeededTrans _ = False

-- Задача 3 -----------------------------------------
transitionsFrom :: Automation -> State -> [Transition]
transitionsFrom (_, _, transs) s = [x | x@(f, _, _) <- transs, f == s]

-- Задача 4 -----------------------------------------
labels :: [Transition] -> [Label]
labels transs = nub [ l | (_,_,l) <- transs, checkLabel l]
    where checkLabel :: Label -> Bool
          checkLabel Eps = False
          checkLabel (C _) = True

-- Задача 5 -----------------------------------------
acceptsDA :: Automation -> String -> Bool
acceptsDA (start, ends, transs) str = acceptsDA' start str
    where findNeededLabel :: Transition -> State -> Char -> Bool
          findNeededLabel (s, _, C c) st cc = (s == st) && (c == cc)
          findNeededLabel (_,_,_) _ _ = False
          acceptsDA' :: State -> String -> Bool
          acceptsDA' curr [] = elem curr ends -- if eos and ending state then True else False
          acceptsDA' curr (s:st)
            | elem curr ends = False --if not eos but ending state then False
            | otherwise = case find (\x -> findNeededLabel x curr s) transs of
                Just (_,next,_) -> acceptsDA' next st   --if not eos and not ending state
                Nothing         -> False

-- Задача 6 -----------------------------------------
stStep  :: Automation -> State -> Label -> [State]
setStep :: Automation -> [State] -> Label -> [State]
closure :: Automation -> [State] -> [State]

stStep (_, _, transs) st mc = [to | (from, to, l) <- transs, from == st, l == mc]
setStep naut bs mc = nub $ concatMap (\x -> stStep naut x mc) bs
closure naut ss = nub $ sort $ (closureHelper ss []) ++ ss
    where closureHelper :: [State] -> [State] -> [State]
          closureHelper [] visited = visited
          closureHelper ws visited = closureHelper filtered $ filtered ++ visited
            where filtered = filter (\x -> notElem x visited) $ setStep naut ws Eps
            
-- Задача 7 -----------------------------------------
accepts :: Automation -> String -> Bool
accepts naut@(start,_,_) str = accepts' [start] str
    where accepts' :: [State] -> String -> Bool
          accepts' [] _ = False
          accepts' prev [] = not $ null $ filter (\x -> isTerminal naut x) prev
          accepts' prev (c:ss) = accepts' (setStep naut (closure naut prev) (C c)) ss

-- Задача 8 -----------------------------------------
makeNDA :: RE -> Automation
makeNDA re = (1, [2], sort transitionsAll)
  where (transitionsAll, _) = make (simplify re) 1 2 3
make :: RE -> Int -> Int -> Int -> ([Transition], Int) 
make Null beg fin nxt = ([(beg, fin, Eps)], nxt)
make (Term c) beg fin nxt = ([(beg, fin, C c)], nxt)
make (Seq l r) beg fin nxt = let (trans1, st1) = make l beg nxt (nxt + 2)
                                 (trans2, st2) = make r (nxt + 1) fin st1
                             in  ((nxt, nxt + 1, Eps) : trans1 ++ trans2, st2)
make (Alt l r) beg fin nxt = 
    let (trans1, st1) = make l nxt (nxt + 1) (nxt + 4)
        (trans2, st2) = make r (nxt + 2) (nxt + 3) (st1)
    in  ((beg, nxt, Eps) : (nxt + 1, fin, Eps) : 
            (beg, nxt + 2, Eps) : (nxt + 3, fin, Eps) : trans1 ++ trans2, st2)
make (Rep r) beg fin nxt = let (trans1, st1) = make r nxt (nxt + 1) (nxt + 2)
                           in  ((beg, nxt, Eps):(beg, fin, Eps):
                                (nxt + 1, fin, Eps):(nxt + 1, nxt, Eps):trans1,st1)
make (Plus _) _ _ _ = undefined
make (Opt _) _ _ _ = undefined

-- Задача 9 -----------------------------------------
{- 
9.	Функція  parseReg st, котра виконує синтаксичний аналіз рядка st, 
розпізнаючи регулярний вираз - значення типу  RE. (Можна використати модуль  Parsec). 
Наприклад: 
•	parseReg “(a?)a” = Just (Seq (Opt (Term ‘a’)) (Term ‘a’))
•	parseReg “ab(+)” = Nothing  

data RE = Null   |
          Term Char |
          Seq RE RE |
          Alt RE RE |
          Rep RE    |
          Plus RE   |
          Opt RE
        deriving (Eq, Show)
-}
parseReg :: String -> Maybe RE 
parseReg str = let filtered = filter (\x -> (notElem x "\t\n ")) str
               in case P.parse reg "" filtered of
                    Left _ -> Nothing
                    Right s -> Just s

reg :: P.Parser RE
reg = do r <- rexpr
         P.eof
         return r;

rexpr :: P.Parser RE
rexpr = do s <- rterm
           s2 <- P.many (rexpr')
           if null s2 then return s else return (Alt s (altRecursively s2))
           
altRecursively :: [RE] -> RE
altRecursively [] = error "Can't reach here"
altRecursively [r] = r
altRecursively (r:rs) = Alt r (altRecursively rs)

rexpr' :: P.Parser RE
rexpr' = do _ <- P.char '|'
            r <- rterm
            return r

rterm :: P.Parser RE
rterm = do r1 <- P.many1 rfact
           return (seqRecursively r1)

seqRecursively :: [RE] -> RE
seqRecursively [] = error "Can't reach here"
seqRecursively [r] = r
seqRecursively (r:rs) = Seq r (seqRecursively rs)

rfact :: P.Parser RE
rfact = do p <- prime
           op <- P.many (P.oneOf "*?+")
           return (opRecursively p (reverse op))

opRecursively :: RE -> String -> RE
opRecursively r [] = r
opRecursively r (o:ops)
    | o == '+' = Plus (opRecursively r ops)
    | o == '*' = Rep (opRecursively r ops)
    | otherwise = Opt (opRecursively r ops)

prime :: P.Parser RE
prime = (P.try rsymb) P.<|> prime'

prime' :: P.Parser RE
prime' = do _ <- P.char '('
            r <- rexpr
            _ <- P.char ')'
            return r

rsymb :: P.Parser RE
rsymb = do ch <- P.noneOf "()|*+?"
           return (Term ch) 

-- Задача 10 -----------------------------------------
{- 
10.	Функція  makeDA nda, котра перетворює НСА nda в еквівалентний детермінований автомат. 
Для реалізації можна визначити допоміжну функцію makeDA’, що визначена в допоміжному файлі. 
Наприклад  (з точністю до нумерації станів детермінованого автомату): 
•	makeDA ndaFigure = daFigure
•	makeDA nda1 = da1
•	makeDA nda3 = da3
-}

makeDA' :: Automation -> (MetaState, [MetaState], [MetaTransition])
makeDA' a = (head l1, l1, l3)
     where (l1,_,l3) = until condD (stepD a) ([],[isx a],[])
           isx at@(at1,_,_) = filter (\x -> isEssential at x) (closure a [at1])
           condD (_,bmsx,_) = null bmsx
           stepD aut (gmsx,bmsx,mtrx) = addStates initM msx mlx l
                     where msx = head bmsx
                           l = nub $ concatMap (\x -> labels $ transitionsFrom aut x) msx
                           bigMlx = map (\x -> closure aut (setStep aut msx x)) l
                           mlx = map (\x -> filter (isEssential aut) x) bigMlx
                           initM = (gmsx ++ [msx], tail bmsx, mtrx)
                           addStates (g,b,m) ms ml ll
                                |null ml = (g,b,m)
                                |otherwise = addStates (g, b' ,m') ms (tail ml) (tail ll) 
                                     where b' = if elem hm b || elem hm g then b else b ++ [hm]
                                           m' = m ++ [(ms, hm, head ll)]
                                           hm = head ml

makeDA :: Automation -> Automation
makeDA (f,t,ls) = (1, tLs, otLs)
     where otLs = sortBy myCompare stLs
           tLs = [x + 1|x <- [0..(length ms) - 1], (not . null) $ intersect (ms !! x) t]
           (_,m2,m3) = makeDA' (f,t,ls)
           ms = m2
           mt = m3
           stLs = [(ind ms x1, ind ms x2, x3) | (x1,x2,x3) <- mt]
           ind xs i = (fromMaybe (-2) $ elemIndex i xs) + 1
           myCompare (a1, b1, _) (a2, b2, _)
                | a1 > a2 = GT
                | a1 < a2 = LT
                | otherwise = compare b1 b2

-------------------------------------------------------
-- showRE - Функція може бути корисною при тестуванні
showRE :: RE -> String
showRE (Seq re re') = showRE re ++ showRE re'
showRE (Alt re re') = "(" ++ showRE re ++ "|" ++ showRE re' ++ ")"
showRE (Rep re)     = showRE' re ++ "*"
showRE (Plus re)    = showRE' re ++ "+"
showRE (Opt re)     =  showRE' re ++ "?"
showRE re           = showRE' re

showRE' :: RE -> String
showRE' Null      = ""
showRE' (Term c)  = [c]
showRE' (Alt re re') = showRE (Alt re re')
showRE' re        = "(" ++ showRE re ++ ")"

--------------------------------------------------------
-- Тестові приклади
reFigureS, re1S, re2S, re3S, re4S, re5S, re6S :: String
reFigureS = "(a|b)*c"
re1S = "(x|y)(1|2)"
re2S = "x'*"
re3S = "(ab|c)*"
re4S = "(a?)a"
re5S = "(ab)?d+"
re6S = "c?*"

reFigure, re1, re2, re3, re4, re5, re6 :: RE
reFigure = Seq (Rep (Alt (Term 'a') (Term 'b'))) (Term 'c')
re1 = Seq (Alt (Term 'x') (Term 'y')) (Alt (Term '1') (Term '2'))
re2 = Seq (Term 'x') (Rep (Term '\''))
re3 = Rep (Alt (Seq (Term 'a') (Term 'b')) (Term 'c'))
re4 = Seq (Opt(Term 'a')) (Term 'a')
re5 = Seq (Opt (Seq (Term 'a') (Term 'b'))) (Plus (Term 'd'))
re6 = Rep (Opt (Term 'c'))

ndaFigure, nda1, nda2, nda3, nda4, nda5, nda6, ndaTest :: Automation
daFigure, da1, da2, da3, da4, da5, da6 :: Automation
ndaFigure
  = (1,[2],[(1,3,Eps),(1,5,Eps),(3,4,Eps),(4,2,C 'c'),(5,7,Eps),
            (5,9,Eps),(6,3,Eps),(6,5,Eps),(7,8,C 'a'),(8,6,Eps),
            (9,10,C 'b'),(10,6,Eps)])
daFigure
  = (1,[2],[(1,1,C 'a'),(1,1,C 'b'),(1,2,C 'c')])

nda1 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,9,Eps),(4,11,Eps),
            (5,6,C 'x'),(6,3,Eps),(7,8,C 'y'),(8,3,Eps),(9,10,C '1'),
            (10,2,Eps),(11,12,C '2'),(12,2,Eps)])
da1 = (1,[3],
     [(1,2,C 'x'),(1,2,C 'y'),(2,3,C '1'),(2,3,C '2')])

nda2 = (1,[2],[(1,3,C 'x'),(3,4,Eps),(4,2,Eps),(4,5,Eps),(5,6,C '\''),
            (6,2,Eps),(6,5,Eps)])
da2 = (1,[2],
     [(1,2,C 'x'),(2,2,C '\'')])

nda3 = (1,[2],[(1,2,Eps),(1,3,Eps),(3,5,Eps),(3,7,Eps),(4,2,Eps),
            (4,3,Eps), (5,9,C 'a'),(6,4,Eps),(7,8,C 'c'),(8,4,Eps),
            (9,10,Eps),(10,6,C 'b')])
da3 = (1,[1],
     [(1,1,C 'c'),(1,2,C 'a'),(2,1,C 'b')])

nda4 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,2,C 'a'),(5,6,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps)])
da4 = (1,[2,3],[(1,2,C 'a'),(2,3,C 'a')])

nda5 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,11,C 'd'),(5,9,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps),(9,10,Eps),(10,6,C 'b'),
            (11,12,Eps),(12,2,Eps),(12,13,Eps),(13,14,C 'd'),
            (14,2,Eps),(14,13,Eps)])
da5 = (1,[2],[(1,2,C 'd'),(1,3,C 'a'),(2,2,C 'd'),(3,4,C 'b'),
            (4,2,C 'd')])

nda6 = (1,[2], [(1,2,Eps),(1,3,Eps),(3,5,Eps),(5,6, C 'c'), (6,4,Eps), 
                (4,2,Eps), (3,7,Eps), (7,8,Eps), (8,4,Eps), (4,3,Eps)]) 
da6 = (1,[1], [(1,1, C 'c')])

ndaTest = (1, [1], [(1,2, C 'a'), (1,4, Eps), (1,3, C 'b'), (2,3, Eps),
              (3,5, Eps), (3,4, C 'a'), (4,4, Eps), (4,1, Eps), (5,2, Eps), (5,4,Eps)] )
