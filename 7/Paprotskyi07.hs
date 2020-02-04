{-# OPTIONS_GHC -Wall #-}
module Paprotskyi07 where
import Data.List(sort)

data BinTreeM a = EmptyM 
                | NodeM a Int (BinTreeM a) (BinTreeM a)
                   deriving (Show, Eq) 
-- B-дерево порядка t (NodeB kl tl) =>  
--      t-1 <= length kl <= 2*t-1  &&  t <= length tl <= 2*t
data Btree a =  NodeB [a] [Btree a]  deriving (Show, Eq)
-- головні характеристики B-дерево  (BInform heigth min max)
data BInform a = BInform {hB::Int, minB::a, maxB::a} deriving (Show, Eq)

-- Задача 1 ------------------------------------
isSearch :: (Ord a) => BinTreeM a -> Bool
isSearch EmptyM = True
isSearch (NodeM v k bl br) 
    | k <= 0 = False
    | otherwise = compareV v bl br && isSearch bl && isSearch br
          where compareV :: (Ord a) => a -> BinTreeM a -> BinTreeM a -> Bool
                compareV _ (EmptyM) (EmptyM) = True
                compareV vv (EmptyM) (NodeM vr _ _ _) = vv < vr
                compareV vv (NodeM vl _ _ _) (EmptyM) = vv > vl
                compareV vv (NodeM vl _ _ _) (NodeM vr _ _ _) = vv > vl && vv < vr
                
-- Задача 2 ------------------------------------
elemSearch :: (Ord a) => BinTreeM a -> a -> Bool
elemSearch EmptyM _ = False
elemSearch (NodeM v _ bl br) val
    | v == val  = True
    | val < v   = elemSearch bl val
    | otherwise = elemSearch br val

-- Задача 3 ------------------------------------
insSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a 
insSearch EmptyM val = NodeM val 1 EmptyM EmptyM
insSearch (NodeM v k bl br) val
    | val == v  = NodeM v (k+1) bl br
    | val < v   = NodeM v k (insSearch bl val) br
    | otherwise = NodeM v k bl (insSearch br val)

-- Задача 4 ------------------------------------
delSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a 
delSearch EmptyM _ = EmptyM
delSearch (NodeM v k EmptyM EmptyM) val                 --leaf
    | val == v && k > 1 = NodeM v (k-1) EmptyM EmptyM
    | otherwise         = EmptyM
delSearch (NodeM v k EmptyM br) val                     --one child right
    | val == v && k > 1  = NodeM v (k-1) EmptyM br
    | val == v && k == 1 = br
    | val > v            = NodeM v k EmptyM (delSearch br val)
    | otherwise          = NodeM v k EmptyM br
delSearch (NodeM v k bl EmptyM) val                     --one child left
    | val == v && k > 1  = NodeM v (k-1) bl EmptyM
    | val == v && k == 1 = bl
    | val > v            = NodeM v k bl EmptyM
    | otherwise          = NodeM v k (delSearch bl val) EmptyM
delSearch curr@(NodeM v k bl br) val                         --two childs
    | val == v && k > 1  = NodeM v (k-1) bl br
    | val > v            = NodeM v k bl (delSearch br val)
    | val < v            = NodeM v k (delSearch bl val) br
    | otherwise          =
        let inSucc = inorderSuccessor br --leftmost value bigger than current value
                where inorderSuccessor :: BinTreeM a -> BinTreeM a
                      inorderSuccessor EmptyM = error "Right undertree can't be empty"
                      inorderSuccessor (NodeM sv sk EmptyM EmptyM) = NodeM sv sk EmptyM EmptyM
                      inorderSuccessor (NodeM sv sk EmptyM sbr)    = NodeM sv sk EmptyM sbr
                      inorderSuccessor (NodeM _ _ sbl _)       = inorderSuccessor sbl
            succDel :: (Ord a) => BinTreeM a -> BinTreeM a -> BinTreeM a -- succ|curr
            succDel EmptyM _ = error "Inorder Successor can't be empty"
            succDel _ EmptyM = error "Input tree can't be empty"
            succDel (NodeM _ _ (NodeM _ _ _ _) _) _ = error "InordSuccessor can't have left child"
            succDel (NodeM svv skk EmptyM _) (NodeM _ _ cbl cbr) = 
                NodeM svv skk cbl $ until cond step cbr
                    where --cond :: BinTreeM a -> Bool
                          cond tree = not (elemSearch tree svv)
                          --step :: BinTreeM a -> BinTreeM a
                          step ctree = delSearch ctree svv
        in  succDel inSucc curr

-- Задача 5 ------------------------------------
sortList :: (Ord a) => [a] -> [a]
sortList [] = []
sortList xs = 
    let tree = foldl insSearch EmptyM xs
        symGoThrough :: (Ord a) => BinTreeM a -> [a]
        symGoThrough EmptyM = []
        symGoThrough (NodeM x k l r) = symGoThrough l ++ (multiCopy x k) ++ symGoThrough r
    in symGoThrough tree

multiCopy :: (Ord a) => a -> Int -> [a]
multiCopy _ 0   = []
multiCopy val 1 = [val]
multiCopy val k = [val] ++ multiCopy val (k-1)

-- Задача 6 ------------------------------------
findBInform :: (Bounded a, Ord a) => Btree a ->  BInform a
findBInform (NodeB [] _)  = error "Key list can't be empty"
findBInform (NodeB ks []) = BInform 0 (head ks) (last ks)
findBInform t = BInform (height t) (lm t) (rm t)
    where height :: Btree a -> Int
          height (NodeB _ []) = 0
          height (NodeB _ ts) = 1 + (height $ head ts)
          lm :: Btree a -> a
          lm (NodeB (k:_) []) = k
          lm (NodeB _ ts) = lm $ head ts
          rm :: Btree a -> a
          rm (NodeB ks []) = last ks
          rm (NodeB _ ts)  = rm $ last ts

-- Задача 7 ------------------------------------
-- B-дерево порядка t (NodeB kl tl) =>  
--      t-1 <= length kl <= 2*t-1  &&  t <= length tl <= 2*t
-- data Btree a =  NodeB [a] [Btree a]  deriving (Show, Eq)
-- головні характеристики B-дерево  (BInform heigth min max)
-- data BInform a = BInform {hB::Int, minB::a, maxB::a} deriving (Show, Eq)

isBtree  :: (Bounded a, Ord a) => Int -> Btree a -> Bool 
isBtree t tr@(NodeB ks ts) 
    | t < 2 = error "t must be >= 2"
    | otherwise = checkRoot && (checkNodesSort tr) && (checkNodesKeysLength ts)
            && (checkKeysRange tr)
        where checkRoot :: Bool                         -- 2
              checkRoot = n >= 1 && n <= (2*t-1) && (length ts) == (n+1)
                  where n = length ks
              checkNodesSort :: (Bounded a, Ord a) => Btree a -> Bool   --4
              checkNodesSort (NodeB kks tts) = isSorted kks && checkAllNodesSort tts
                  where checkAllNodesSort []     = True
                        checkAllNodesSort (x:xs) = checkNodesSort x && checkAllNodesSort xs
              checkNodesKeysLength :: [Btree a] -> Bool  --1
              checkNodesKeysLength [] = True
              checkNodesKeysLength ((NodeB kkks ttts):xs) = (kn <= (2*t-1)) && (kn >= (t-1)) &&
                        (checkNodesKeysLength ttts) && (checkNodesKeysLength xs)
                  where kn = length kkks
              checkKeysRange (NodeB krs trs) = (checkKeySetRange krs trs) && (checkKeysRangeAll trs)
                  where checkKeysRangeAll [] = True
                        checkKeysRangeAll (x:xxxs) = (checkKeysRange x) && (checkKeysRangeAll xxxs)
                        checkKeySetRange [] _ = True
                        checkKeySetRange _ [] = True
                        checkKeySetRange (kk:kkrs) (tt:ttrs) = (checkSingleKeyRange kk (tt)) 
                                && (checkKeySetRange kkrs ttrs)
                        checkSingleKeyRange kkk (NodeB nbk _) = null $ filter (\z -> z > kkk) nbk
              
isSorted :: (Ord a) => [a] -> Bool
isSorted []       = True
isSorted [_]      = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)

-- Задача 8 ------------------------------------
eqBtree :: (Bounded a, Ord a) => Int -> Btree a -> Btree a -> Bool 
eqBtree _ f s = sort (treeToString f) == sort (treeToString s)

treeToString :: Btree a -> [a]
treeToString (NodeB keys strees) = keys ++ (concatMap treeToString strees)

-- Задача 9 ------------------------------------
elemBtree :: Ord a => Btree a -> a -> Bool
elemBtree t x = elem x $ treeToString t

position :: Ord a => a -> [a] -> Int
position v xs = case [ind | (ind, x) <- zip [0..] xs, v <= x] of
    []      -> 0
    (x:_) -> x

-- Задача 10 ------------------------------------
insBtree :: Ord a => Int -> Btree a -> a -> Btree a
insBtree t tree v  
    | (isFull t tree) = 
        let (lt, uk, rt) = splitAtB t tree
        in insertIntoNode t (NodeB [uk] [lt,rt]) v
    | otherwise = insertIntoNode t tree v

isFull :: Ord a => Int -> Btree a -> Bool
isFull t (NodeB ks _ ) = length ks == 2*t-1

insertIntoNode :: Ord a => Int -> Btree a -> a -> Btree a
insertIntoNode _ (NodeB ks []) v = NodeB (insertKey v ks) []
insertIntoNode t (NodeB ks ts) v =
    let (kl1,kl2,tl1,bt,tl2) = decomposeNodeB v ks ts
    in if (isFull t bt) then
                let (bt1,k,bt2) = splitAtB t bt
                    btr1 = if v <= k then (insertIntoNode t bt1 v) else bt1
                    btr2 = if v <= k then bt2 else (insertIntoNode t bt2 v)
                in NodeB (kl1 ++ (k:kl2)) (tl1 ++ (btr1:(btr2:tl2)))
       else NodeB ks (tl1 ++ ((insertIntoNode t bt v):tl2))


insertKey :: Ord a => a -> [a] -> [a]
insertKey v [] = [v]
insertKey v (x:xs)
    | v <= x = v:(x:xs)
    | otherwise = x:(insertKey v xs)

decomposeNodeB :: Ord a => a -> [a] -> [Btree a] -> 
                        ([a], [a], [Btree a], Btree a, [Btree a])
decomposeNodeB v ks ts =
    let pos = position v ks
        tl1 = [ts!!ti | ti <- [0..(pos-1)]]
        tl2 = [ts!!ti | ti <- [pos+1..(length ts)-1]]
        kl1 = [ks!!kind | kind <- [0..(pos-1)]]
        kl2 = [ks!!kind | kind <- [pos..(length ks)-1]]
        bt = ts!!pos
    in (kl1,kl2,tl1,bt,tl2)
        

splitAtB :: Ord a => Int -> Btree a -> (Btree a, a, Btree a)
splitAtB t (NodeB ks ts) = 
    let ksl = [ks!!i | i <- [0..(t-2)]]
        tsl = if null ts then [] else [ts!!i | i <- [0..t-1]]
        ksr = [ks!!i | i <- [t..(2*t-1)]]
        tsr = if null ts then [] else [ts!!i | i <- [t..(2*t-1)]]
    in (NodeB ksl tsl, ks!!(t-1), NodeB ksr tsr)

---------------------Тестові дані - Дерева пошуку -------
bm :: BinTreeM Char
bm = NodeM  't' 2  
            (NodeM 'a' 1  EmptyM 
                    (NodeM 'e' 1 
                             (NodeM 'd' 2 EmptyM EmptyM)
                             (NodeM 'f' 1 EmptyM EmptyM)
                    )
            ) 
            (NodeM 'w' 2  EmptyM EmptyM)   
{-
tiB1 :: Btree Char 
tiB1 = NodeB ['G','M','P','X'] 
             [ NodeB ['A','C','D','E'] []
             , NodeB ['J','K'] []
             , NodeB ['N','O'] []
             , NodeB ['R','S','T','U','V'] []
             , NodeB ['Y','Z'] [] ]

tBtr1 :: Btree Int
tBtr1 = NodeB [5,10,12] [ts0,ts1,ts2,ts3]
   where ts0 = NodeB [1,3  ] []   --- ,4,5] []  --
         ts1 = NodeB [6,6 ,8,9,10] [] --- ,8,9,10] []  -- ] []   
         ts2 = NodeB [11,11,12,12] []
         ts3 = NodeB [16,16] [] -- ,18,19,20] [] 

tBtr2 :: Btree Int 
tBtr2 = NodeB [15] [ts10,ts11]
  where ts10 = NodeB [11,13] [] 
        ts11 = NodeB [21,22] []  
-}
tBt1 :: Btree Char 
tBt1 = NodeB "L"
       [ NodeB "DG" 
          [ NodeB "AC" [], NodeB "EE" [], NodeB "HK" []
          ]
       , NodeB "PU" 
          [ NodeB "MM" [], NodeB "RS" [], NodeB "UW" []
          ]
       ]

tBt2 :: Btree Char 
tBt2 = NodeB "GP"
       [ NodeB "ACDEE" [], NodeB "HKLMM" [], NodeB "RSUUW" []
       ]

tBt5 :: Btree Char 
tBt5 = NodeB "GMPX"
       [ NodeB "ACDE" [] , NodeB "JK" [], NodeB "NO" []
       , NodeB "RSTUV" [], NodeB "YZ" []
       ]

tBt6 :: Btree Char 
tBt6 = NodeB "GMPX"
       [ NodeB "ABCDE" [], NodeB "JK" [], NodeB "NO" []
       , NodeB "RSTUV" [], NodeB "YZ" []
       ]

tBt7 :: Btree Char 
tBt7 = NodeB "GMPTX"
       [ NodeB "ABCDE" [], NodeB "JK" [], NodeB "NO" []
       , NodeB "QRS" [], NodeB "UV" [], NodeB "YZ" []
       ]

tBt8 :: Btree Char 
tBt8 = NodeB "P"
       [ NodeB "GM"
          [ NodeB "ABCDE" [], NodeB "JKL" [], NodeB "NO" []
          ]
       , NodeB "TX" 
          [ NodeB "QRS" [], NodeB "UV" [], NodeB "YZ" []
          ]
       ]

tBt9 :: Btree Char 
tBt9 = NodeB "P"
       [ NodeB "CGM"
          [ NodeB "AB" [], NodeB "DEF" []
          , NodeB "JKL" [], NodeB "NO" []
          ]
       , NodeB "TX" 
          [ NodeB "QRS" [], NodeB "UV" [], NodeB "YZ" []
          ]
       ]
