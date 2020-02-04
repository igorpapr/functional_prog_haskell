{-# OPTIONS_GHC -Wall #-}
module Paprotskyi09 where
import Data.List(delete)

-- розглядаємо лише цілі дані: скаляри  і масиви  
--------------------------------------------------------------------
type Id    = String
data Value = I Int | A [(Int, Int)]  deriving (Eq, Show)
data Op    = Add | Minus | Mul | Less | Equal | Index  deriving (Eq, Show)

data Exp = Const Int 
         | Var Id 
         | OpApp Op Exp Exp 
         | Cond Exp Exp Exp 
         | FunApp Id [Exp] 
         deriving (Eq, Show)

data Stmt = Assign Id Exp 
          | AssignA Id Exp Exp 
          | If Exp Stmt Stmt 
          | While Exp Stmt 
          | Call Id [Exp] 
          | Block [VarDef] [Stmt]
          deriving (Eq, Show)

data VarDef  =  Arr Id | Int Id deriving (Eq, Show)

type FunDef  =  (Id, ([VarDef], Exp))
-- функції повертають лише цілі скалярні дані, не використовують глобальні дані (чисті!!)
type ProcDef = (Id, ([VarDef], Stmt))
type Program = ([VarDef], [FunDef], [ProcDef])

type StateP  = [(Id, Value)]  -- стек даних

data Type    = At | It  deriving (Eq, Show)
type FunEnv  = [(Id,[Type])]
type ProcEnv = [(Id,[Type])]
type VarEnv  = [(Id,Type)]

-- Задача 1 ------------------------------------
updateValue :: Eq a => a -> b -> [(a,b)] -> [(a,b)]
updateValue x y [] = [(x,y)]
updateValue x y (a:abss)
    | fst a == x = [(fst a, y)] ++ abss
    | otherwise = [a] ++ (updateValue x y abss)
    
-- Задача 2 ------------------------------------
updateArray :: Value -> Value -> Value -> Value
updateArray (A a) (I i) (I v) = A (updateValue i v a)
updateArray _ _ _ = error "Bad arguments"

-- Задача 3 ------------------------------------
normalniyLookUp :: Eq a => [(a,b)] -> a -> Maybe b
normalniyLookUp arr x =
    let res = [y | y <- arr, (fst y) == x]
    in case null res of
        False  -> Just $ snd $ head res
        True -> Nothing

applyOp :: Op -> Value -> Value -> Value 
applyOp Add (I x) (I y) = I(x + y)
applyOp Minus (I x) (I y) = I(x - y)
applyOp Mul (I x) (I y) = I(x * y)
applyOp Less (I x) (I y) 
    | x < y = I 1
    | otherwise = I 0
applyOp Equal (I x) (I y)
    | x == y = I 1
    | otherwise = I 0
applyOp Index (A a) (I i) = case normalniyLookUp a i of
    Just v  -> I v
    Nothing -> I 0
applyOp _ _ _ = error "Bad arguments"

-- Задача 4 ------------------------------------
evExp ::  Exp -> [FunDef] -> StateP -> Value 
evExp (Const i) _ _ = I i
evExp (Var v) _ st = lookUp v st
evExp (OpApp o x y) dfs st = applyOp o (evExp x dfs st) (evExp y dfs st)
evExp (Cond p f s) dfs st
    | (evExp p dfs st) == I 0 = evExp s dfs st
    | otherwise = evExp f dfs st
evExp (FunApp idd es) dfs st =
    let (vars, ef) = lookUp idd dfs
        vs = evArgs es dfs st
        new = evState vars vs
    in evExp ef dfs new
        
evState :: [VarDef] -> [Value] -> StateP
evState vd v = zip (map getName vd) v

getName :: VarDef -> Id
getName (Arr i) = i
getName (Int i) = i

evArgs :: [Exp] -> [FunDef] ->StateP -> [Value]  
evArgs ex dfx st = [v | x <- ex , let v = (evExp x dfx st) ]

-- Задача 5 ------------------------------------
evStmt :: Stmt -> [FunDef] -> [ProcDef] -> StateP -> StateP
evStmt (Assign n ex) dfx _ st = let i = evExp ex dfx st
                                  in updateValue n i st
evStmt (AssignA n ex1 ex2) dfx _ st = updateValue n (updateArray arr f s) st
                                            where f = evExp ex1 dfx st
                                                  s = evExp ex2 dfx st
                                                  arr = lookUp n st
evStmt (If ex stm1 stm2) dfx dpx st = 
    evStmt (if i /= 0 then stm1 else stm2) dfx dpx st
        where I i = evExp ex dfx st
     
evStmt (While ex stm) dfx dpx st = until cond step st
                                   where 
                                        cond stt = let I i = evExp ex dfx stt
                                                   in i == 0 
                                        step stt = evStmt stm dfx dpx stt
evStmt (Call n es) dfx dpx st =  
       let (par, proc) = lookUp n dpx
           ans = evStmt proc dfx dpx (updState n es dfx dpx st)
       in  deleteLocal par ans
evStmt (Block vd stm) dfx dpx st = 
       let updSt = map initv vd ++ st
           ans = snd $ until cond0 step0 (stm,updSt)
                       where cond0 (stm0,_) = null stm0
                             step0 (stm0,st0) =
                                     (tail stm0, evStmt (head stm0) dfx dpx st0)
       in deleteLocal vd ans

deleteLocal::[VarDef] -> StateP -> StateP
deleteLocal vd state = snd $ until cond1 step1 (vd,state)
                       where cond1 (v, _) = null v
                             step1 (v, a) = ((tail v),(deleteVarInState (head v) a))

updState:: Id -> [Exp] -> [FunDef] ->[ProcDef] -> StateP -> StateP
updState n es dfx dpx st = let (par, _) = lookUp n dpx
                               vars = map (\x -> evExp x dfx st) es
                       in evState par vars ++ st

deleteVarInState ::VarDef -> StateP -> StateP
deleteVarInState (Arr i) st = delete (i, lookUp i st) st
deleteVarInState (Int i) st = delete (i, lookUp i st) st

-- Задача 6 ------------------------------------
iswfOp1 :: Op -> [Maybe Type] -> Maybe Type 
iswfOp1 Add   [(Just It),(Just It)] = Just It
iswfOp1 Minus [(Just It),(Just It)] = Just It
iswfOp1 Mul   [(Just It),(Just It)] = Just It
iswfOp1 Less  [(Just It),(Just It)] = Just It
iswfOp1 Equal [(Just It),(Just It)] = Just It
iswfOp1 Index [(Just At),(Just It)] = Just It
iswfOp1 _      _      = Nothing

iswfExp :: Exp -> VarEnv -> FunEnv -> Maybe Type   
iswfExp (Const _) _ _ = Just It
iswfExp (Var v) ve _ = case normalniyLookUp ve v of
    Nothing -> Nothing
    Just t  -> Just t
iswfExp (OpApp op e1 e2) ve fe = (iswfOp1 op [(iswfExp e1 ve fe),(iswfExp e2 ve fe)])
iswfExp (Cond e1 e2 e3) z zz = case ((iswfExp e1 z zz),(iswfExp e2 z zz),(iswfExp e3 z zz)) of
    (Just It, Just s1, Just s2) -> if s1 == s2 then Just s1 else Nothing
    _                           -> Nothing
iswfExp (FunApp n es) ve fe = let ft = map Just (lookUp n fe)
                                  est = map (\x -> iswfExp x ve fe) es
                              in if ft == est then Just It else Nothing 
-- Задача 7 ------------------------------------
iswfStmt :: Stmt -> VarEnv -> FunEnv -> ProcEnv -> Bool
iswfStmt (Assign n ex) ve fe _ = lookup n ve == iswfExp ex ve fe && 
                                   lookup n ve == Just It
iswfStmt (AssignA n ex1 ex2) ve fe _ = iswfAssignA [f, s, t]
       where Just f = lookup n ve
             Just s = iswfExp ex1 ve fe
             Just t = iswfExp ex2 ve fe
iswfStmt (If ex st1 st2) ve fe pe = 
       Just It == iswfExp ex ve fe && 
       iswfStmt st1 ve fe pe &&
       iswfStmt st2 ve fe pe
iswfStmt (While ex st) ve fe pe = 
       Just It == iswfExp ex ve fe &&
       iswfStmt st ve fe pe
iswfStmt (Call n vs) ve fe pe =
       map (\x -> iswfExp x ve fe) vs == (map Just (lookUp n pe))
iswfStmt (Block vd sts) ve fe pe = all (\x -> iswfStmt x veNew fe pe) sts
    where veNew = map initvType vd ++ ve

initvType :: VarDef -> (Id, Type)
initvType (Arr v) = (v, At)
initvType (Int v) = (v, It)

-- Задача 8 ------------------------------------
iswfFunDef :: FunDef -> FunEnv -> Bool
iswfFunDef fd fe = elem idTypes fe && iswfExp ex ve fe /= Nothing
                        where (fn,(ft,ex)) = fd
                              idTypes = (fn, map initType ft)
                              ve = map initvType ft

iswfProcDef :: ProcDef -> VarEnv -> FunEnv -> ProcEnv -> Bool
iswfProcDef pd ve fe pe = let (pname,(ptypes, st)) = pd
                              ptypesUpd = map initType ptypes
                              veUpd = map initvType ptypes ++ ve
                              peUpd = updateValue pname ptypesUpd pe 
                          in elem (pname, ptypesUpd) peUpd && 
                            iswfStmt st veUpd fe peUpd

initType :: VarDef -> Type
initType (Arr _) = At
initType (Int _) = It

-- Задача 9 ------------------------------------
createEnv :: [(Id, ([VarDef], a))] -> [(Id, [Type])]
createEnv fd = map initFenv fd 
                  where initFenv (fn, (fargs, _)) = (fn, map initType fargs)

iswfProgram :: Program -> Bool
iswfProgram (vd, fd, pd) = let veN = map initvType vd
                               feN = createEnv fd
                               peN = createEnv pd
                               procMain ps = any (\(pname,_) -> pname=="main") ps
                           in all (\x -> iswfFunDef x feN) fd &&
                              all (\x -> iswfProcDef x veN feN peN) pd && 
                              procMain peN && uniqN veN feN peN

uniqN :: VarEnv -> FunEnv -> ProcEnv -> Bool
uniqN ve fe pe = let getAllNames e = map (\(name, _) -> name) e
                     names = (getAllNames ve) ++ (getAllNames fe) ++ (getAllNames pe)
                 in uniq names

uniq :: Eq a => [a]  -> Bool
uniq [] = True
uniq (x:xs) =  notElem x xs && uniq xs

--- Допоміжні функції -----------------------------
lookUp :: Eq a => a -> [(a,b)] -> b
-- Передумова: Пара з ключом a є в списку пар abx
lookUp a abx = maybe (error "lookUp") id (lookup a abx) 

-- формує початкове значення змінної
initv :: VarDef -> (Id, Value)
initv (Arr v) = (v, A [])
initv (Int v) = (v, I 0) 

-- Реалізація виконання програми 
evProgram :: Program -> StateP 
evProgram (dvx, dfx, dpx) = 
   let sb = map initv dvx 
       ( _, s) = lookUp "main" dpx      
   in  evStmt s dfx dpx sb   

--  iswfOp o ts - перевіряє коректність типів операндів ts 
--     бінарної операції o і формує тип результату Just t або Nothing  
iswfOp :: Op -> [Type] -> Maybe Type 
iswfOp Add   [It,It] = Just It
iswfOp Minus [It,It] = Just It
iswfOp Mul   [It,It] = Just It
iswfOp Less  [It,It] = Just It
iswfOp Equal [It,It] = Just It
iswfOp Index [At,It] = Just It
iswfOp _      _      = Nothing

--  iswfCond ts - перевіряє коректність  типів операндів ts
--     умовного виразу і формує тип результату Just t або Nothing 
iswfCond :: [Type] -> Maybe Type 
iswfCond [It,It,It] = Just It
iswfCond [It,At,At] = Just At
iswfCond _          = Nothing 

-- iswfAssignA ts перевіряє коректність  типів операндів ts
--   операції присвоювання значення елементу масива 
iswfAssignA :: [Type] -> Bool
iswfAssignA [At,It,It] = True 
iswfAssignA _          = False  

---- Дані для тестування  -----------------------
-- Стан для тестування
sampleState :: StateP
sampleState = [("x",I 5),("y",I 2),("a", A [(2,3),(0,4), (1,2)])]

varEnv :: VarEnv 
varEnv = [("x",It), ("y",It), ("a",At)]

-- Функція максимум двох чисел 
-- func biggest(m,n)= (m<n ? n : m)
biggest :: FunDef
biggest =("biggest",
          ([Int "m", Int "n"], 
           Cond (OpApp  Less (Var "m") (Var "n"))  (Var "n")  (Var "m")                                                                
           )
         )
-- Функція, що обчислює число Фібоначчі
-- func fib(n) = (n<3 ? 1 : fib(n-1) + fib(n-2))
fib :: FunDef
fib = ("fib",
       ([Int "n"], 
        Cond (OpApp Less (Var "n") (Const 3))
             (Const 1)
             (OpApp Add (FunApp "fib" [OpApp Minus (Var "n") (Const 1)])
                        (FunApp "fib" [OpApp Minus (Var "n") (Const 2)]))
       )
      )

-- Функція - сума елементів масиву 0..n ...
-- func sumA(a[],n) = (n<0 ? 0 : a[n] + sumA (a,n-1))
sumA :: FunDef
sumA = ("sumA",
        ([Arr "a", Int "n"],
         Cond (OpApp Less (Var "n") (Const 0)) 
              (Const 0)
              (OpApp Add (OpApp Index (Var "a") (Var "n"))
                         (FunApp "sumA" [Var "a", OpApp Minus (Var "n")(Const 1)])
              )
        )
       )

funEnv :: FunEnv
funEnv = [("biggest",[It,It]),("fib", [It]),("sumA",[At,It])]

-- Приклад оператору - блоку 
sampleBlock :: Stmt 
sampleBlock = Block [Arr "b"]
                 [AssignA "b" (Const 0) (Const 9), AssignA "b" (Const 2) (Const 5),
                  AssignA "b" (Const 3) (Const 7), AssignA "b" (Const 5) (Const 1),
                  Call "sumA1" [Var "b", Const 5]
                 ]

-- Процедура - додавання двох чисел...
-- proc gAdd(x,y) gSum = x + y 
gAdd :: ProcDef
gAdd = ("gAdd", 
        ([Int "x", Int "y"], 
         Assign "gSum" (OpApp Add (Var "x") (Var "y"))
        )
       )

-- Процедура - сума елементів масиву 0..n ...
-- proc sumA1(a[],n) {i;limit;
--      sA=0; i=0; limit=n+1;
--      while (i<limit){sA=sA+a[i]; i=i+1}
--                   }
sumA1 :: ProcDef
sumA1 = ("sumA1",
         ([Arr "a", Int "n"], 
          Block [Int "i", Int "limit"] 
            [Assign "sA" (Const 0), Assign "i" (Const 0),
             Assign "limit" (OpApp Add (Var "n") (Const 1)),
             While (OpApp Less (Var "i") (Var "limit"))
                   (Block [] 
                     [Assign "sA" (OpApp Add (Var "sA")
                                  (OpApp Index (Var "a") (Var "i"))),
                      Assign "i" (OpApp Add (Var "i") (Const 1))
                     ]
                   )
            ]
         )
        )

procEnv :: ProcEnv 
procEnv = [("gAdd",[It,It]),("sumA1",[At,It])]

-- Повні програми
-- gSum; 
-- proc gAdd(x,y) gSum = x + y 
-- proc main() call gAdd(5,10)   
pr1 :: Program
pr1 = ([Int "gSum"], [], [gAdd, ("main",([],Call "gAdd" [Const  5, Const 10]))])

-- sA
-- proc sumA1(a[],n) {i;limit; .... } 
-- proc main() {b[]; b[0]=9; b[2]=5; b[3]=7; b[5]=1;
--                   call sumA1 (b,5)
--             }

pr2 :: Program
pr2 = ([Int "sA"], [], 
       [sumA1, 
        ("main",([], sampleBlock))
       ])
