{-# OPTIONS_GHC -Wall #-}
module Paprotskyi05 where

import Data.Char(isUpper)
import Data.List

type Grammar    = [Production]         -- КВ-граматика
type Production = (Char,String)        -- Правило виводу
type Predict    = [(Char,String)]      -- Прогнозуюча таблиця
type Control    = [((Char,Char),Int)]  -- Управляюча таблиця 

-- Задача 1 ------------------------------------
--Функція addOne st c - додає до множини-рядка st символ c.
--Функція addAll st wd - додає до множини-рядка st символи з рядка wd, повертаючи  множину-рядок  (wd - не обов"язково множина-рядок).
--Функція addWithout st wd – схожа на попередню але символ '$' з рядка wd НЕ додається (ігнорується).
--Функція inter st1 st2 - реалізує перетин двох множин-рядків st1 i st2, повертаючи множину-рядок.
addOne :: String -> Char -> String  
addOne st c | elem c st = st
            | otherwise = sort (st ++ [c])

addAll :: String -> String -> String 
addAll st [] = st
addAll st (w:wd) = sort (addAll (addOne st w) wd)

addWithout :: String -> String -> String 
addWithout st wd = sort (addAll st (filter (/= '$') wd))

inter :: String -> String -> String 
inter [] _    = []
inter _ []    = []
inter st1 st2 = sort [x | x <- st1, elem x st2]

-- Задача 2 ------------------------------------
-- => Функція tkPredict pt n вибирає з прогнозуючої таблицi pt  множину, 
--що  зв`язана з нетерміналом n. 
--Якщо нетермінал n відсутній, то повертається "" - порожня множина.
-- => Функція upPredict pt n st змінює в таблиці pt множину,  зв`язану з нетерміналом n, на рядок-множину st. 
--Якщо нетермінал n відсутній, то в таблицю pt додається пара (n,st).
tkPredict :: Predict -> Char -> String
tkPredict [] _ = "" 
tkPredict (p:pt) n | (fst p) == n = snd p
                   | otherwise    = tkPredict pt n

upPredict :: Predict -> Char -> String -> Predict
upPredict [] n st = [(n, st)]
upPredict (p:pt) n st 
    | (n == fst p) = (n, st):pt
    | (n < fst p)  = (n, st):p:pt
    | otherwise = p:(upPredict pt n st)

-- Задача 3 ------------------------------------
--Функція parse gr ctl word, що моделює роботу LL(1)-аналізатора  
--для граматики gr з управляючою таблицею ctl на слові word, повертаючи Just il, де  il- список номерів продукцій, 
--що задають лівосторонній вивід слова  word, або Nothing - якщо слово word НЕ належить мові граматики  gr. 
--Можна додатково визначити функцію step gr ctl (input, staсk, result), 
--що описує один крок роботи LL(1)-аналізатора з таблицею ctl на конфігурації (input, staсk, result).

-- gr0 = [('S',"aAS"),('S',"b"), ('A',"a"), ('A',"bSA")]  
-- ctl0 = [(('A','a'),2),(('A','b'),3),(('S','a'),0),(('S','b'),1)]
--	parse gr0 ctl0 "abbab” = Just [0,3,1,2,1]
--	parse gr0 ctl0 "aaa” = Nothing


parse ::  Grammar -> Control -> String -> Maybe [Int]
parse [] _ _ = error "Empty Grammar"
parse _ [] _ = error "Empty Control"
parse (g:gr) ctl word =
    let (_, _, res) = until cond stepHelper ((word++"$"), (fst g):"$", Just [])
                          where 
                              stepHelper :: (String, String, Maybe [Int]) -> (String, String, Maybe [Int])
                              stepHelper (inputH,stackH,resH) = step (g:gr) ctl (inputH,stackH,resH)
                              cond :: (String, String, Maybe [Int]) -> Bool
                              cond (ic, sc, rc) = (((ic == "$")&&(sc == "$")) || (rc == Nothing))
    in res

step :: Grammar -> Control -> 
       (String, String, Maybe [Int]) -> (String, String, Maybe [Int])
step _  _   (input, stack, Nothing) = (input, stack, Nothing)
step _  _   ("$", "$", Just result) = ("$", "$", Just result)
step _  _   ([], _, Just _) = error "Word can't be empty"
step _  _   (_, [], Just _) = error "Stack can't be empty"
step gr ctl ((i:input), (s:stack), Just result)
    | (not (isUpper i))&& (not (isUpper s)) && (i == s) && (i /='$') = (input, stack, Just result)
  --  | not (isUpper s) = ((i:input), (s:stack), Nothing)--can be ERROR!!!!!!!!!!!!!!!
    | otherwise =  --table_cross - komirka from table, ((Char,Char),Int)
        let tableCross = find (\x -> ((fst(fst x) == s) && (snd(fst x) == i))) ctl
            createStepResult :: Maybe ((Char,Char),Int) -> (String, String, Maybe [Int])
            createStepResult Nothing = ((i:input),(s:stack), Nothing)
            createStepResult (Just ((_,_),index)) = ((i:input),(snd(gr!!index))++stack,Just(result++[index]))
        in  createStepResult tableCross

-- Задача 4 ------------------------------------
-- Функція first pFst st, котра визначає множину початкових термінальних символів,
-- котрі можна вивести з слова st (pFst - прогнозуюча таблиця початкових терміналів)

-- Алгоритм обчислення функції first: базується на використанні таблиці fst.
--	Якщо α=ε,  то first(α) = {$}
--	Якщо α=aβ, a Є VT, то first(α) = {a}
--	Якщо α=A,  A Є VN, то first(α) = fst(A)
--	Якщо α=Aβ, A Є VN  i $ !Є fst(A), то first(α) = fst(A)
--	Якщо α=Aβ, A Є VN  i $ Є fst(A),  то first(α) = (fst(A) – {$}) U first(β)

first :: Predict -> String -> String
first [] _ = error "Empty fst table"
first _ "" = "$"
first pFst (a:st)
    | not (isUpper a) = [a]
    | otherwise = sort (firstNotTerminal (a:st))
        where firstNotTerminal :: String -> String
              firstNotTerminal [] = error "Empty string argument"
              firstNotTerminal (aa:[]) = case find (\x -> (fst x) == aa) pFst of
                  Just x  -> snd x
                  Nothing -> error "Couldn't find N in table"
              firstNotTerminal (aa:stt) | not (elem '$' (getSecondFromMaybe(find (\x -> (fst x) == aa) pFst))) = getSecondFromMaybe(find (\x -> (fst x) == aa) pFst)

                                        | otherwise = addAll (addWithout "" (getSecondFromMaybe(find (\x -> (fst x) == aa) pFst))) (first pFst stt)
              getSecondFromMaybe :: Maybe (Char,String) -> String
              getSecondFromMaybe Nothing = error "Couldn't find N in table"
              getSecondFromMaybe (Just (_,s)) = s 

-- Задача 5 ------------------------------------
-- Функція buildingControl gr pFst pNxt, що будує управляючу таблицю 
-- для LL(1)-граматики gr  з прогнозуючими таблицями pFst  початкових і pNxt  наступних терміналів.

--	Для правила виводу A -> α з номером i
-- o	Якщо a Є VT і a Є first(α), то σ(A,a) = i,
-- o	Якщо ε Є first(α), то для довільного b Є nxt(A), [то] σ(A,b) = i.
-- •	Для всіх інших значень B Є VN , b Є VT U {$},  σ(B,b) = E. -- тобто нема


buildingControl :: Grammar -> Predict -> Predict -> Control 
buildingControl [] _ _ = error "Empty Grammar"
buildingControl _ [] _ = error "Empty First Table"
buildingControl _ _ [] = error "Empty Next Table"
buildingControl gr pFst pNxt = sort (concatMap (\x -> makeControlListFromCurrentProduction x pFst pNxt) (zip [0..] gr))
makeControlListFromCurrentProduction :: (Int,(Production)) -> Predict -> Predict -> [((Char,Char),Int)] 
makeControlListFromCurrentProduction (i,(ch,alpha)) ppFst ppNxt
    | elem '$' (first ppFst alpha) = [((ch, b), i) | b <- tkPredict ppNxt ch]
    | otherwise = [((ch, a),i) | a <- (first ppFst alpha), not (isUpper a)]

-- Задача 6 ------------------------------------
--  Функція testingLL1 gr pFst pNxt, що перевіряє, чи є граматика gr з  
--  прогнозуючими таблицями pFst – початкових і pNxt наступних терміналів - LL(1)-граматикою.  
-- Для реалізації можна додатково визначити функції: 
--  => fromGrammar gr - групує всі продукції граматики gr в список пари (n,rls), де  n - нетермінал граматики 
--  і rls - ВСІ праві частини продукцій, що виводяться з n, 
--	=> testFst rls - перевіряє 1 властивість для одного з нетерміналів граматики
--  - з різних правих частин його продукцій rls виводяться різні початкові термінали. 
--	=> testFollow fs rls – перевіряє 2  властивість для одного з нетерміналів граматики, 
--  коли з одної з продукцій нетерміналу rls виводиться порожнє слово ( fs - наступні символи для цього нетерміналу).
--Використовуючи функції first,  nxt і попереднє означення LL(1) граматики  отримуємо наступний критерій:  
--G – LL(1) граматика тоді і тільки тоді, коли для кожного нетерміналу A Є VN  
--і всіх правил виводу A -> α1, …, A -> αn Є P виконуються дві умови 
--1.	first(αi) intersect first(αj)  = {},  1 ≤ i  ≠ j  ≤ n.
--2.	якщо існує αi  *=> ε, то first(αj) intersect nxt(A)  = {},  1 ≤ i  ≠ j  ≤ n.

--	testingLL1 gr1 pFst1 pNxt1 = True
--	testingLL1 gr4 pFst4 pNxt4 = False

testingLL1 :: Grammar -> Predict -> Predict -> Bool
testingLL1 gr pFst pNxt = check (fromGrammar gr)
    where check :: [(Char, [String])] -> Bool
          check [] = error "Empty grammar"
          check [x] = (testFst (snd x) pFst) && (testFollow (tkPredict pNxt (fst x)) (snd x) pFst)
          check (x:xs) = (testFst (snd x) pFst) && (testFollow (tkPredict pNxt (fst x)) (snd x) pFst) && (check xs)

fromGrammar :: Grammar ->  [(Char,[String])]
fromGrammar gr = fltrDuplicates [(n, rls) | (n, _) <- gr, let rls = getRls gr n]
-- function to filter duplicates. SORTS the order
fltrDuplicates :: (Ord a) => [a] -> [a]
fltrDuplicates = map head . group . sort
--creates list of ALL right sides of grammar which are output from neterminal
getRls :: Grammar -> Char -> [String]
getRls gr n = [s | x <- (filter (\y -> (fst y) == n) gr), let s = snd x]

testFst :: [String] -> Predict -> Bool
testFst [] _ = error "Bad input into testFst"
testFst _ [] = error "Empty FirstTable"
testFst (_:[]) _ = True
testFst (rls) pFst = null (fullFirstInter rls)
    where fullFirstInter :: [String] -> String
          fullFirstInter []  = []
          fullFirstInter [x] = first pFst x
          fullFirstInter (x:xs) = inter (first pFst x) (fullFirstInter xs)

testFollow :: String -> [String] -> Predict -> Bool --fs - next symbols (taken from nxt(A))
testFollow [] _ _ = error "Empty NextTable Symbols"
testFollow _ [] _ = error "Empty right side string"
testFollow fs rls pFst | elem "" rls = 
                            let newrls = delete "" rls
                                interAllWithNext :: [String] -> String -> Bool
                                interAllWithNext [] _ = True
                                interAllWithNext [s] ns = null(inter (first pFst s) ns)
                                interAllWithNext (s:ss) ns = (null(inter (first pFst s) ns)) && interAllWithNext ss ns
                            in interAllWithNext newrls fs
                       | otherwise = True

-- Задача 7 ------------------------------------
--7.	Функція buildFst gr, котра будує для граматики gr прогнозуючу таблицю початкових терміналів.  
-- Для реалізації можна додатково визначити функції: 
-- ==>	evalFst gr pFst - будує послідовність наближень прогнозуючої таблиці pFst
-- ==>	extandFst pFst (n,rul) - розширює прогнозуючу таблицю pFst, обробляючи продукцію (n,rul). 

buildFst :: Grammar -> Predict 
buildFst [] = error "Empty grammar has come as an argument"
buildFst gr = fst (until (\x -> not (snd x)) stepFst ((createFst0 gr), True))
    where stepFst :: (Predict, Bool) -> (Predict,Bool) -- Bool - to continue until or not (whether the previous result == current result) (check must be done in evalFst)
          stepFst (pr0, cont) = evalFst gr (pr0, cont)
          
createFst0 :: Grammar -> Predict
createFst0 gr = [(processFst0FromGrammarItem x) | x <- (fromGrammar gr)]
    where processFst0FromGrammarItem :: (Char,[String]) -> (Char, String)
          processFst0FromGrammarItem (n, srsides)
              | elem "" srsides = (n,"$")
              | otherwise       = (n, "")
    
evalFst :: Grammar -> (Predict,Bool) -> (Predict, Bool) 
evalFst gr (pr, _) =
    let newpr = processExtending gr pr
        processExtending :: Grammar -> Predict -> Predict
        processExtending [] ppr = ppr
        processExtending (gg:ggr) ppr = processExtending ggr (extandFst ppr gg) 
    in if (pr == newpr) then (newpr, False) else (newpr, True)

extandFst :: Predict -> Production -> Predict 
extandFst pr (ch, s) = upPredict pr ch (sort (addAll (first pr (s)) (tkPredict pr ch)))

-- Задача 8 ------------------------------------

--8. Функція buildNxt gr pFst, що будує для граматики gr прогнозуючу таблицю наступних терміналів, 
-- використовуючи ВЖЕ побудовану таблицю початкових терміналів pFst. 
-- Для реалізації можна додатково визначити функції: 
-- ==> • nontermTails gr  - будує ВСІ нетермінальні "хвости" продукцій граматики gr.
-- ==> • evalNxt tails pFst pNxt - будує послідовність наближень прогнозуючої таблиці наступних терміналів pNxt, 
--   використовуючи: tails - ВСІ нетерміналні "хвости" продукцій граматики і pFst  - прогнозуючу таблицю початкових терміналів.
-- ==> • extandNxtOne pFst n pNxt (m:st) - розширює прогнозуючу таблицю pNxt обробляючи "хвіст" продукції граматики  (n,m:st).

--Rules to compute FOLLOW set:
-- 1) FOLLOW(S) = { $ }   // where S is the starting Non-Terminal
-- 2) If A -> pBq is a production, where p, B and q are any grammar symbols,
--   then everything in FIRST(q)  except $ is in FOLLOW(B).
-- 3) If A->pB is a production, then everything in FOLLOW(A) is in FOLLOW(B).
-- 4) If A->pBq is a production and FIRST(q) contains $, 
--   then FOLLOW(B) contains { FIRST(q) – $ } U FOLLOW(A) 

buildNxt :: Grammar -> Predict -> Predict
buildNxt [] _ = error "Empty grammar has come as an argument"
buildNxt _ [] = error "Empty fst table has come as an argument"
buildNxt gr pFst = fst (until (\x -> not (snd x)) stepNxt ((createNxt0 gr), True))
    where stepNxt :: (Predict, Bool) -> (Predict, Bool) -- Bool - to continue until or not (whether the previous result == current result) (check must be done in evalNxt)
          stepNxt (pr0, cont) = evalNxt (nontermTails gr) pFst (pr0, cont)

createNxt0 :: Grammar -> Predict
createNxt0 gr = 
    let fstnonterm = fst (head gr)
        dataFromGrammar = fromGrammar gr
    in sort ([(n, "$") | (n, _) <- dataFromGrammar, n == fstnonterm] ++ [(nn, "") | (nn, _) <- (deleteBy (\_ y -> (fst y) == fstnonterm) (' ',[""]) dataFromGrammar)])

nontermTails :: Grammar -> [(Char,String)]
nontermTails gr = concatMap (getTailsFromSingleProduction) gr
    where getTailsFromSingleProduction :: Production -> [(Char,String)]
          getTailsFromSingleProduction (ch, s) = [(ch, t) | (ind, symbol) <- (zip [0..] s), isUpper symbol, let t = drop ind s]

evalNxt :: [(Char,String)] -> Predict -> (Predict, Bool) -> (Predict, Bool)
evalNxt tails' pFst (pNxt, _) =
    let newpr = processExtending tails' pNxt
        processExtending :: [(Char, String)] -> Predict -> Predict
        processExtending [] pr = pr
        processExtending ((n, ta):ts) pr = processExtending ts (extandNxtOne pFst n pr ta)
    in if (pNxt == newpr) then (newpr, False) else (newpr, True)

extandNxtOne :: Predict -> Char -> Predict -> String -> Predict
extandNxtOne _ _ _ [] = error "Tail is empty"
extandNxtOne pFst n pNxt (m:st)
    | st == [] = upPredict pNxt m (sort (addAll (tkPredict pNxt m) (tkPredict pNxt n)))
    | elem '$' (first pFst st) = upPredict pNxt m (sort (addAll (tkPredict pNxt m) (addAll (addWithout (first pFst st) "") (tkPredict pNxt n))))
    | otherwise = upPredict pNxt m (sort (addWithout (tkPredict pNxt m ) (first pFst st)))

---------------------Тестові дані ---------------------------
 
gr0, gr1, gr2, gr3, gr4, gr5:: Grammar
--  LL(1)-граматики
gr0 = [('S',"aAS"),('S',"b"), ('A',"a"), ('A',"bSA")]  
gr1 = [('S',"TV"),('T',"d"),('T',"(S)"),('V',"+TV"),('V',"-TV"),('V',"")]  
gr2 = [('E',"TU"),('U',""),('U',"+TU"),('U',"-TU"),
       ('T',"FV"),('V',""),('V',"*FV"),('V',"%FV"),('V',"/FV"),
       ('F',"d"),('F',"(E)")] 
-- не LL(1)-граматики
gr3 = [('S',"aAS"), ('S',"a"),('A',"SbA"),('A',"ba"),('S',"")]
gr4 = [('E',"E+T"),('E',"T"), ('T',"T*F"), ('T',"F"), ('F',"d"),('F',"(E)") ]   
gr5 = [('E',"E+T"), ('E',"E-T"),('E',"T"), 
       ('T',"T*F"), ('T',"T%F"), ('T',"T/F"), ('T',"F"), 
       ('F',"d"),('F',"(E)") ]

-- прогнозуючі таблиці початкових терміналів Fst
pFst0, pFst1, pFst2, pFst3, pFst4, pFst5 :: Predict
pFst0 = [('A',"ab"),('S',"ab")]
pFst1 = [('S',"(d"),('T',"(d"),('V',"$+-")]
pFst2 = [('E',"(d"),('F',"(d"),('T',"(d"),('U',"$+-"),('V',"$%*/")]
pFst3 = [('A',"ab"),('S',"$a")]
pFst4 = [('E',"(d"),('F',"(d"),('T',"(d")]
pFst5 = [('E',"(d"),('F',"(d"),('T',"(d")]

-- прогнозуючі таблиці наступних терміналів Nxt
pNxt0, pNxt1, pNxt2, pNxt3, pNxt4, pNxt5 :: Predict
pNxt0 = [('A',"ab"),('S',"$ab")]
pNxt1 = [('S',"$)"),('T',"$)+-"),('V',"$)")]
pNxt2 = [('E',"$)"),('F',"$%)*+-/"),('T',"$)+-"),('U',"$)"),('V',"$)+-")]
pNxt3 = [('A',"$ab"),('S',"$b")]
pNxt4 = [('E',"$)+"),('F',"$)*+"),('T',"$)*+")]
pNxt5 = [('E',"$)+-"),('F',"$%)*+-/"),('T',"$%)*+-/")]   

-- управляючі таблиці 
ctl0, ctl1, ctl2 :: Control 
ctl0 = [(('A','a'),2),(('A','b'),3),(('S','a'),0),(('S','b'),1)]
ctl1 = [(('S','('),0),(('S','d'),0),(('T','('),2),(('T','d'),1),
        (('V','$'),5),(('V',')'),5),(('V','+'),3),(('V','-'),4)]
ctl2 = [(('E','('),0),(('E','d'),0),(('F','('),10),(('F','d'),9),
        (('T','('),4),(('T','d'),4),(('U','$'),1),(('U',')'),1),
        (('U','+'),2),(('U','-'),3),(('V','$'),5),(('V','%'),7),
        (('V',')'),5),(('V','*'),6),(('V','+'),5),(('V','-'),5),(('V','/'),8)]
