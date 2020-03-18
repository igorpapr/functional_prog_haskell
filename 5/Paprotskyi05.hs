{-# OPTIONS_GHC -Wall #-}
module Paprotskyi05 where

import Data.Char(isUpper)
import Data.List

type Grammar    = [Production]         -- Context-free grammar
type Production = (Char,String)        -- Rule of output
type Predict    = [(Char,String)]      -- prognosis table
type Control    = [((Char,Char),Int)]  -- managing(control) table 

-- Задача 1 ------------------------------------
--addOne st c - adds to set-string st symbol c.
--addAll st wd - adds to set-string st symbols from string wd, returning set-string
--  (wd - not necessarily set-string).
--addWithout st wd – similiar to previous but '$' from string wd is being IGNORED.
--inter st1 st2 - intersection of two set-strings st1 and st2, returning set-string.
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

-- Task 2 ------------------------------------
-- => Function tkPredict pt n chooses from prognosis table pt set,
-- which is connected with neterminal n.
--if neterminal n is absent, "" is returned (empty set).
-- => Function upPredict pt n st replaces in table pt set, which is connected with neterminal n,
-- with string-set st. 
--If neterminal n is absent, pair (n, st) is being added to the table pt.
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

-- Task 3 ------------------------------------
--parse gr ctl word, modelling LL(1)-analisys  
--for grammar gr with control table ctl on the string word, returning Just il, 
--where  il - list of production numbers, 
--with describe left-hand output of the string word, or Nothing - if string word DOESN'T belong to 
--language of grammar gr. 
--We can additionaly define the function step gr ctl (input, staсk, result), 
--which describes one step of LL(1)-analiser work with table ctl 
--using the configuration (input, staсk, result).

-- gr0 = [('S',"aAS"),('S',"b"), ('A',"a"), ('A',"bSA")]  
-- ctl0 = [(('A','a'),2),(('A','b'),3),(('S','a'),0),(('S','b'),1)]
--	parse gr0 ctl0 "abbab” = Just [0,3,1,2,1]
--	parse gr0 ctl0 "aaa” = Nothing


parse ::  Grammar -> Control -> String -> Maybe [Int]
parse [] _ _ = error "Empty Grammar"
parse _ [] _ = error "Empty Control"
parse (g:gr) ctl word =
    let (_, _, res) = 
        until cond stepHelper ((word++"$"), (fst g):"$", Just [])
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
    | otherwise =  --table_cross - cell from table, ((Char,Char),Int)
        let tableCross = find (\x -> ((fst(fst x) == s) && (snd(fst x) == i))) ctl
            createStepResult :: Maybe ((Char,Char),Int) -> (String, String, Maybe [Int])
            createStepResult Nothing = ((i:input),(s:stack), Nothing)
            createStepResult (Just ((_,_),index)) = 
                ((i:input),(snd(gr!!index))++stack,Just(result++[index]))
        in  createStepResult tableCross

-- Task 4 ------------------------------------
-- Function first pFst st, which defines a set of starting(first) terminal symbols,
-- that can be output from word st (pFst - prognosis table of starting(first) terminals)

-- Algorythm of computation of function FIRST: basing on the starting table fst.
--	If α=ε,  then first(α) = {$}
--	If α=aβ, a Є VT, then first(α) = {a}
--	If α=A,  A Є VN, then first(α) = fst(A)
--	If α=Aβ, A Є VN  and $ !Є fst(A), then first(α) = fst(A)
--	If α=Aβ, A Є VN  and $ Є fst(A),  then first(α) = (fst(A) – {$}) U first(β)

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

-- Task 5 ------------------------------------
-- Function buildingControl gr pFst pNxt, witch builds control table 
-- for LL(1)-grammar gr  with prognosis table pFst of starting(first) and pNxt following terminals.

--	For production rule A -> α with number i
-- o	If a Є VT and a Є first(α), then σ(A,a) = i,
-- o	If ε Є first(α), then for any b Є nxt(A), [then] σ(A,b) = i.
-- •	For all other values B Є VN , b Є VT U {$},  σ(B,b) = E. -- that is, there is none


buildingControl :: Grammar -> Predict -> Predict -> Control 
buildingControl [] _ _ = error "Empty Grammar"
buildingControl _ [] _ = error "Empty First Table"
buildingControl _ _ [] = error "Empty Next Table"
buildingControl gr pFst pNxt = sort (concatMap (\x -> makeControlListFromCurrentProduction x pFst pNxt) (zip [0..] gr))
makeControlListFromCurrentProduction :: (Int,(Production)) -> Predict -> Predict -> [((Char,Char),Int)] 
makeControlListFromCurrentProduction (i,(ch,alpha)) ppFst ppNxt
    | elem '$' (first ppFst alpha) = [((ch, b), i) | b <- tkPredict ppNxt ch]
    | otherwise = [((ch, a),i) | a <- (first ppFst alpha), not (isUpper a)]

-- Task 6 ------------------------------------
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

---------------------Testing values ---------------------------
 
gr0, gr1, gr2, gr3, gr4, gr5:: Grammar
--  LL(1)-grammars
gr0 = [('S',"aAS"),('S',"b"), ('A',"a"), ('A',"bSA")]  
gr1 = [('S',"TV"),('T',"d"),('T',"(S)"),('V',"+TV"),('V',"-TV"),('V',"")]  
gr2 = [('E',"TU"),('U',""),('U',"+TU"),('U',"-TU"),
       ('T',"FV"),('V',""),('V',"*FV"),('V',"%FV"),('V',"/FV"),
       ('F',"d"),('F',"(E)")] 
-- not LL(1)-grammar
gr3 = [('S',"aAS"), ('S',"a"),('A',"SbA"),('A',"ba"),('S',"")]
gr4 = [('E',"E+T"),('E',"T"), ('T',"T*F"), ('T',"F"), ('F',"d"),('F',"(E)") ]   
gr5 = [('E',"E+T"), ('E',"E-T"),('E',"T"), 
       ('T',"T*F"), ('T',"T%F"), ('T',"T/F"), ('T',"F"), 
       ('F',"d"),('F',"(E)") ]

-- prognosis tables of starting terminals Fst
pFst0, pFst1, pFst2, pFst3, pFst4, pFst5 :: Predict
pFst0 = [('A',"ab"),('S',"ab")]
pFst1 = [('S',"(d"),('T',"(d"),('V',"$+-")]
pFst2 = [('E',"(d"),('F',"(d"),('T',"(d"),('U',"$+-"),('V',"$%*/")]
pFst3 = [('A',"ab"),('S',"$a")]
pFst4 = [('E',"(d"),('F',"(d"),('T',"(d")]
pFst5 = [('E',"(d"),('F',"(d"),('T',"(d")]

-- prognosis tables of following terminals Nxt
pNxt0, pNxt1, pNxt2, pNxt3, pNxt4, pNxt5 :: Predict
pNxt0 = [('A',"ab"),('S',"$ab")]
pNxt1 = [('S',"$)"),('T',"$)+-"),('V',"$)")]
pNxt2 = [('E',"$)"),('F',"$%)*+-/"),('T',"$)+-"),('U',"$)"),('V',"$)+-")]
pNxt3 = [('A',"$ab"),('S',"$b")]
pNxt4 = [('E',"$)+"),('F',"$)*+"),('T',"$)*+")]
pNxt5 = [('E',"$)+-"),('F',"$%)*+-/"),('T',"$%)*+-/")]   

-- control tables
ctl0, ctl1, ctl2 :: Control 
ctl0 = [(('A','a'),2),(('A','b'),3),(('S','a'),0),(('S','b'),1)]
ctl1 = [(('S','('),0),(('S','d'),0),(('T','('),2),(('T','d'),1),
        (('V','$'),5),(('V',')'),5),(('V','+'),3),(('V','-'),4)]
ctl2 = [(('E','('),0),(('E','d'),0),(('F','('),10),(('F','d'),9),
        (('T','('),4),(('T','d'),4),(('U','$'),1),(('U',')'),1),
        (('U','+'),2),(('U','-'),3),(('V','$'),5),(('V','%'),7),
        (('V',')'),5),(('V','*'),6),(('V','+'),5),(('V','-'),5),(('V','/'),8)]
