{-# OPTIONS_GHC -Wall #-}
module Paprotskyi08 where
import Data.List(find, nub)
import Text.ParserCombinators.Parsec

data Recur = Zero | Succ | Sel Int Int
           | Super Recur [Recur]
           | Prim Recur Recur
           | Mini Recur Int
           | Name String  deriving (Show, Eq)
type System = [(String,Recur)]

-- Задача 1 ------------------------------------
isNumbConst :: System -> Recur -> Bool
isNumbConst _ Zero = True
isNumbConst syst (Super Succ [f]) = isNumbConst syst f
isNumbConst syst (Name nm) =
    let res = getFuctionByName syst nm
        incHelper :: Maybe (String, Recur) -> Bool
        incHelper Nothing = error "Couldn't find the function in the system"--False
        incHelper (Just (_, fh)) = isNumbConst syst fh
    in incHelper res
isNumbConst _ _ = False

getFuctionByName :: System -> String -> Maybe (String,Recur)
getFuctionByName syst name = find (\x -> name == (fst x)) syst

-- Задача 2 ------------------------------------
evRank :: System -> Recur -> Int
evRank _ Zero = 1
evRank _ Succ = 1
evRank _ (Sel n _ ) = n
evRank syst (Super _ al) = evRank syst (head al)
evRank syst (Prim _ st) = (evRank syst st) - 1
evRank syst (Mini b _) = (evRank syst b) - 1
evRank syst (Name nm) =
    let res = getFuctionByName syst nm
        rankHelper :: Maybe (String, Recur) -> Int
        rankHelper Nothing = -1--error "Couldn't find a function with that name"
        rankHelper (Just (_, fh)) = evRank syst fh
    in rankHelper res

-- Задача 3 ------------------------------------
isNames :: System -> Bool
isNames syst = uniqueNames syst && (uniqueUsings (zip [0..] syst))
    where uniqueNames :: System -> Bool
          uniqueNames s = length (nub s) == length s
          uniqueUsings :: [(Int, (String, Recur))] -> Bool
          uniqueUsings [] = error "Empty system came in as an argument"
          uniqueUsings ss = null [(i,(name,f))|(i, (name, f))<-ss,((maxIndOfUsedFunc f ss)>=i)]
          maxIndOfUsedFunc :: Recur -> [(Int, (String, Recur))] -> Int
          maxIndOfUsedFunc r sys = maximum $ indsUsedFunc r sys
          fWithIndByName sss nm = find (\(_,(b,_)) -> nm == b) sss
          indsUsedFunc :: Recur -> [(Int,(String,Recur))]->[Int]
          indsUsedFunc (Super y ys) z = (indsUsedFunc y z) ++
                (concatMap (\k -> indsUsedFunc k z) ys)
          indsUsedFunc (Prim l r) z = (indsUsedFunc l z) ++ (indsUsedFunc r z)
          indsUsedFunc (Mini y _) z = indsUsedFunc y z
          indsUsedFunc (Name n) z = case (fWithIndByName z n) of
                Nothing -> error "Couldn't find a function with that name"
                (Just (index, (_,_))) -> index:[]
          indsUsedFunc _ _ = (-1):[]

-- Задача 4 ------------------------------------
isRecur :: System -> Recur -> Bool
isRecur _ Zero = True
isRecur _ Succ = True
isRecur _ (Sel a one) = (a>= 1) && (one>=1) && (one<=a)
isRecur syst (Super g gs) =
    let allRanksSame = null (filter (\y -> not $ isRecur syst y) gs)
        allCorrect = null [x | x <- gs, evRank syst x /= evRank syst g]
    in allRanksSame && allCorrect
isRecur syst (Prim g h)
    | (evRank syst h) > 2  = ((evRank syst g) == (evRank syst h - 2)) &&
        isRecur syst g && isRecur syst h
    | (evRank syst h) == 2 = (evRank syst g) == 1 &&
        isRecur syst g && isRecur syst h
    | otherwise = False
isRecur syst (Mini g _) = (evRank syst g > 1) && (isRecur syst g)
isRecur syst (Name str) = case getFuctionByName syst str of
    Nothing    -> False
    Just (_,_) -> True

-- Задача 5 ------------------------------------
eval :: System -> Recur -> [Int] -> Int
eval _ (Mini _ _) _ = error "Mini is a part-recur function, use evalPart function instead"
eval syst (Name str) v = case (getFuctionByName syst str) of
    Nothing  -> error "Couldn't find a function with that name in the system"
    Just (_,res) -> eval syst res v
eval _ (Zero) v
    | (length v) /= 1 = error "Incorrect number of arguments"
    | otherwise = 0
eval _ (Succ) v
    | (length v) /= 1 = error "Incorrect number of arguments"
    | otherwise = (head v)+1
eval syst r@(Sel _ k) v
    | (length v) /= (evRank syst r) = error "Incorrect number of arguments"
    | otherwise = v!!(k-1)
eval syst r@(Super f fs) v
    | (length v) /= (evRank syst r) = error "Incorrect number of arguments"
    | otherwise = eval syst f (map (\x -> eval syst x v) fs)
eval syst r@(Prim g h) v
    | (length v) /= (evRank syst r) = error "Incorrect number of arguments"
    | otherwise = evalPrim syst g h v

evalPrim :: System -> Recur -> Recur -> [Int] -> Int
evalPrim syst g h v
    | (length v) == 1 = evalPrimH syst h ((takeWithoutLast v)++(0:[eval syst g v])) (head v)
    | otherwise = evalPrimH syst h ((takeWithoutLast v)++
        (0:[eval syst g (takeWithoutLast v)])) (last v)

-- system | h | v | value to check end
evalPrimH :: System -> Recur -> [Int] -> Int -> Int
evalPrimH syst h v chck
    | (last (takeWithoutLast v)) == chck = last v
    | otherwise =
        let counter = last (takeWithoutLast v)
            args = takeWithoutLast $ takeWithoutLast v
        in evalPrimH syst h (args ++ ((counter+1):[eval syst h v])) chck

takeWithoutLast :: [a] -> [a]
takeWithoutLast [] = []
takeWithoutLast xs = take ((length xs) - 1) xs

-- Задача 6 ------------------------------------
evalPart :: System -> Recur -> [Int] -> Maybe Int
evalPart syst r@Zero v = Just $ eval syst r v
evalPart syst r@Succ v = Just $ eval syst r v
evalPart syst r@(Sel _ _) v = Just $ eval syst r v
evalPart syst r@(Super _ _) v = Just $ eval syst r v
evalPart syst r@(Prim _ _) v = Just $ eval syst r v
evalPart syst (Name str) v = case (getFuctionByName syst str) of
    Nothing -> error "Couldn't find a function with that name in the system"
    Just (_,res) -> evalPart syst res v
evalPart syst r@(Mini g t) v
    | (length v) /= (evRank syst r) = error "Incorrect number of arguments"
    | otherwise = evalMini syst g t (v++[0])
evalMini :: System -> Recur -> Int -> [Int] -> Maybe Int
evalMini syst g t v
    | last v > t = Nothing
    | otherwise = case evalPart syst g v of
        Nothing -> Nothing
        Just 0  -> Just (last v)
        Just _  -> evalMini syst g t ((takeWithoutLast v) ++ [(last v) + 1])

-- Задача 7 ------------------------------------

parseRec :: String -> Maybe System
parseRec str = let filtered = filter (\x -> (notElem x "\t\n ")) str
               in case parse system "" filtered of
                    Left _ -> Nothing
                    Right s -> Just s
                                        
integer :: Parser Int
integer = do ds <- many1 digit
             return $ read ds

idenBase :: Parser Recur
idenBase = do s  <- letter
              nd <- many (digit <|> letter)
              return (Name (s:nd))
         
iden :: Parser String
iden = do s<-letter
          nd <- many (digit <|> letter)
          return (s:nd)

recur :: Parser Recur
recur = base <|> super <|> prim <|> mini
  
base :: Parser Recur
base =(try suc)<|>(try zero)<|>(try sel)<|>idenBase
 

zero :: Parser Recur
zero = do _ <- string "z1"
          return Zero

suc ::Parser Recur
suc = do _ <- string "a1"
         return Succ

sel :: Parser Recur
sel = do _ <- char 's'
         d1 <- digit
         d2 <- digit
         return (Sel (read [d1]) (read [d2]))

superHelper :: Parser Recur
superHelper = do _ <- char ','
                 r <- recur
                 return r

super :: Parser Recur
super = do _ <- char '('
           g <- recur
           _ <- char ':'
           gsf <- recur
           gss <- many (superHelper)
           _ <- char ')'
           return (Super g (gsf:gss))

prim :: Parser Recur
prim = do _ <- char '['
          g <- recur
          _ <- char ','
          h <- recur
          _ <- char ']'
          return (Prim g h)

mini :: Parser Recur
mini = do _ <- char '{'
          f <- recur
          _ <- char ','
          t <- integer
          _ <- char '}'
          return (Mini f t)

system :: Parser System
system = do f<- many fu
            eof
            return f;
            
fu :: Parser (String, Recur)
fu = do i<- iden
        _<- char '='
        r<-recur
        _<-char ';'
        return (i,r)

---------------------Тестові дані -  -------
syst1, syst2 :: System
syst1 = [("const0", Zero)
   , ("const0v2", Super Zero [Sel 2 1])
   , ("const0v3", Super Zero [Sel 3 1])
   , ("const1v2", Super Succ [Super Zero [Sel 2 1]])
   , ("const2", Super Succ [Super Succ [Zero]])
   , ("addition", Prim (Sel 1 1) (Super Succ [Sel 3 3 ]))
   , ("multiplication", Prim Zero (Super (Name "addition") [Sel 3 3, Sel 3 1]))
   , ("notSignum", Prim (Super Succ [Zero]) (Super Zero [Sel 2 1]))
   , ("subtract1", Prim Zero (Sel 2 1))
   , ("subtraction", Prim (Sel 1 1) (Super (Name "subtract1") [Sel 3 3]))
   , ("subtractionRev", Super (Name "subtraction") [Sel 2 2, Sel 2 1])
   , ("subtractionAbs", Super (Name "addition") [Name "subtraction", Name "subtractionRev"])
   , ("subtractionAbs3", Super (Name "subtractionAbs") [Sel 3 1, Super (Name "addition") [Sel 3 2, Sel 3 3]])
   , ("subtractionPart", Mini (Name "subtractionAbs3") 100)
   ]

syst2 = [("f1", Super Succ [Zero])
        ,("f2", Super Succ [Name "f2"])
        ]


sysStr1,sysStr2 :: String
sysStr1 = " const0 = z1; const0v2  = (z1 : s21); const0v3 = (z1:s31);\n\
          \  const1v2 = (a1 : (z1 : s21));  \n\
          \  const2= (a1:(a1:z1)); addition = [s11, (a1:s33)] ;\n\
          \  multiplication = [z1 , (addition: s33,s31)]; \n\
	      \  notSignum = [(a1:z1),(z1:s21)];\n\
		  \  subtract1 = [z1,s21]; subtraction = [s11, (subtract1:s33)];\n\
		  \  subtractionRev = (subtraction : s22, s21);\n\
          \  subtractionAbs = (addition: subtraction, subtractionRev); \n\
          \  subtractionAbs3=(subtractionAbs:s31, (addition:s32,s33))  ;\n \
          \ subtractionPart = {subtractionAbs3, 100 };"

sysStr2 = " f1 = (a1:z1); f2 = (a1, f2);"
