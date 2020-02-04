{-# OPTIONS_GHC -Wall #-}
module Paprotskyi03 where

type Algorithm    = [Substitution]
type Substitution = (String,String,Bool)
type ConfigA      = (Bool, Int, String)

data Command = Z Int | S Int | T Int Int | J Int Int Int deriving Show
type Program = [Command]
type ConfigC = (Int, Int, [Int])

-- Задача 1 ------------------------------------
isPrefix :: String -> String -> Bool
isPrefix [] _ = True
isPrefix _ [] = False
isPrefix (b:bs) (x:xs) | b == x = isPrefix bs xs
                       | otherwise = False

-- Задача 2 ------------------------------------
crop2Parts :: String -> Int -> (String, String)
crop2Parts [] _ = ([],[])
crop2Parts xs ind = (take ind xs, drop ind xs)

substitute :: Substitution -> Int -> String -> String
substitute (ls, rs, _) i w = 
    let (fi, se) = crop2Parts w i
        replaceIfMatch :: String -> String -> String -> String
        replaceIfMatch str l r
            | isPrefix l str = r ++ (drop (length l) str)
            | otherwise = str
    in fi ++ (replaceIfMatch se ls rs)
--if we avoid situations where pattern doesn't match at this index:
--substitute (ls, rs, _) i w = take i w ++ rs ++ drop (i + length ls) w

-- Задача 3------------------------------------
findPosition :: String -> Substitution -> [(Substitution, Int)]
findPosition str (ls, rs, e) = [((ls,rs,e),x) | x <- [0..(length str)], isPrefix ls (drop x str)]

-- Задача 4 ------------------------------------
findAll :: Algorithm -> String -> [(Substitution,Int)]  
findAll [] _ = []
findAll (s:ss) str = findPosition str s ++ findAll ss str

-- Задача 5 ------------------------------------
stepA :: Algorithm -> ConfigA -> ConfigA
stepA _ (False, st, word) = (False, st, word)
stepA algo (_, st, word) = 
    let ((ls,rs,e), i) = head (findAll algo word)
    in (not e, st + 1, substitute (ls, rs, e) i word)

-- Задача 6 ------------------------------------
evalA :: Algorithm -> Int -> String -> Maybe String 
evalA algo m word = 
    let (rb, rn, rw) = until cond step (True, 0, word)
                            where step :: ConfigA -> ConfigA
                                  step (b, n, w) = stepA algo (b, n, w)
                                  cond :: ConfigA -> Bool
                                  cond (bc, nc, _) = (nc >= m ||  bc == False)
    in if ((rn >= m) && (rb == True)) then Nothing else Just rw
-- Задача 7 ------------------------------------
maximReg :: Program -> Int
maximReg [] = 0
maximReg (x:xs) = max (findMaxReg x 0) (maximReg xs)
    where findMaxReg :: Command -> Int -> Int
          findMaxReg (Z a) curr = max a curr
          findMaxReg (S a) curr = max a curr
          findMaxReg (T a b) curr = max (max a b) curr
          findMaxReg (J a b _) curr = maximum (a:(b:[curr]))
-- Задача 8 ------------------------------------
ini :: Program -> [Int] -> [Int] 
ini [] _ = error "The program is empty"
ini prog xs = 
    let maxlen = maximReg prog 
    in if (maxlen >= (length xs)) then xs ++ (take ((maxlen) - (length xs))[0,0..]) else xs

upd :: [Int] -> Int -> Int-> [Int]
upd [] _ _ = []
upd regs r v = (take r regs) ++ v:(drop (r+1) regs)

-- Задача 9 ------------------------------------
stepC :: Program -> ConfigC -> ConfigC
stepC progs (nm, st, reg) =
    let applyCommand :: Command -> ConfigC -> ConfigC
        applyCommand (Z a) (nma, sta, rega) = (nma+1, sta+1, upd rega (a-1) 0)
        applyCommand (S a) (nma, sta, rega) = (nma+1, sta+1, upd rega (a-1) ((rega!!(a-1))+1))
        applyCommand (T a b) (nma, sta, rega) = (nma+1, sta+1, upd rega (b-1) (reg!!(a-1)))
        applyCommand (J a b q) (nma, sta, rega) = if (rega!!(a-1)) == (rega!!(b-1)) then (q, sta+1, rega) else (nma+1,sta+1,rega)
    in applyCommand (progs!!(nm-1)) (nm, st, reg)
-- Задача 10 ------------------------------------
evalC :: Program -> Int -> [Int] -> Maybe Int
evalC [] _ _ = Nothing
evalC prog maxn inireg = 
    let (nm, st, reg) = until cond step (1, 0, (ini prog inireg))
                            where cond :: ConfigC -> Bool
                                  cond (n, s, _) = (s >= maxn || n > (length prog))
                                  step :: ConfigC -> ConfigC
                                  step c = stepC prog c
    in if (nm <= (length prog) && (st >= maxn)) then Nothing else Just (head reg) 

---------------------Тестові дані - Нормальні алгоритми Маркова ---------------------------
clearBeginOne, addEnd, reverse, multiply:: Algorithm 
-- стирає перший символ вхідного слова (алфавіт {a,b})
clearBeginOne = [ ("ca", "", True)
                , ("cb", "", True)
                , ("", "c", False)
                ] 

-- дописує abb в кінець вхідного слова (алфавіт {a,b})
addEnd = [ ("ca", "ac", False)
         , ("cb", "bc", False)
         , ("c", "abb", True)
         , ("", "c", False)
         ] 
-- зеркальне відображення вхідного слова (алфавіт {a,b})
reverse = [ ("cc", "d", False)
          , ("dc", "d", False)
          , ("da", "ad", False) 
          , ("db", "bd", False) 
          , ("d", "", True) 
          , ("caa", "aca", False) 
          , ("cab", "bca", False) 
          , ("cba", "acb", False)
          , ("cbb", "bcb", False) 
          , ("", "c", False) 
          ]

-- добуток натуральних чисел 
--  multiply ("|||#||") = "||||||"  3*2 = 6
multiply = [("a|", "|ba", False)
            ,("a", "", False)
            ,("b|", "|b", False)
            ,("|#", "#a", False)
            ,("#", "c", False)
            ,("c|", "c", False)
            ,("cb", "|c", False)
            ,("c", "", True)
            ]

---------------------Тестові дані - Програми МНР ---------------------------
notSignum, addition, subtraction :: Program 
-- функція notSignum x
notSignum = [Z 2, J 1 2 5, Z 1, J 1 1 6, S 1] 

-- функція додавання  addition x y = x+y
addition = [Z 3, J 3 2 6, S 1, S 3, J 1 1 2]

-- функція віднімання subtraction x y = x-y, визначена для x>=y 
subtraction = [Z 3, J 1 2 6, S 2, S 3, J 1 1 2, T 3 1]