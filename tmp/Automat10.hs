{-# OPTIONS_GHC -Wall #-}
module Automat10 where
import Data.List
type State = Int
data Label = C Char | Eps deriving (Eq, Ord, Show)
type Transition = (State, State, Label)
type Automation = (State, [State], [Transition])

--Знаходить всі стани автомату
states :: Automation -> [State]
states (s, ss, (x,y,_)) = (sort . nub) ((s:ss) ++ (x:[y]))

isDeter :: Automation -> Bool
isDeter (_, _, trx) =
    (null . (filter (\(_,_,l) -> l == Eps))) trx &&
    (null . (filter ((>1).length)).group.sort.map(\(s,_,l) -> (s,l)))) trx

