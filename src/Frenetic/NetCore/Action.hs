module Frenetic.NetCore.Action
  ( Port
  , Forward (..)
  , Action (..)
  , emptyAction
  , unionAction
  , interAction
  , flood
  , forward
  ) where

import Control.Concurrent.Chan
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word

type Port = Word16

data Forward
  = ForwardPorts (Set Port)
  | ForwardFlood 
  deriving (Eq)

instance Show Forward where
  show (ForwardPorts set) = show (Set.toList set)
  show ForwardFlood = "Flood"

data Action = Action {
  actionForwards :: Forward,
  actionNumPktQueries :: [(Chan Word64, Int)]
} deriving (Eq)

instance Show Action where
  show (Action fwd _) = "<fwd=" ++ show fwd ++ ">"

emptyAction :: Action
emptyAction = Action (ForwardPorts Set.empty) []

unionForward :: Forward -> Forward -> Forward
unionForward ForwardFlood _ = ForwardFlood
unionForward _ ForwardFlood = ForwardFlood
unionForward (ForwardPorts set1) (ForwardPorts set2) = 
  ForwardPorts (set1 `Set.union` set2)

interForward :: Forward -> Forward -> Forward
interForward ForwardFlood ForwardFlood = ForwardFlood
interForward ForwardFlood (ForwardPorts set2) = ForwardPorts set2
interForward (ForwardPorts set1) ForwardFlood = ForwardPorts set1
interForward (ForwardPorts set1) (ForwardPorts set2) = 
  ForwardPorts (set1 `Set.intersection` set2)

unionAction :: Action -> Action -> Action
unionAction (Action fwd1 q1) (Action fwd2 q2) = 
  Action (unionForward fwd1 fwd2) (unionQuery q1 q2)
    where unionQuery xs ys = xs ++ filter (\y -> not (y `elem` xs)) ys

interAction :: Action -> Action -> Action
interAction (Action fwd1 q1) (Action fwd2 q2) = 
  Action (interForward fwd1 fwd2) (interQuery q1 q2)
    where interQuery xs ys = filter (\x -> x `elem` ys) xs

flood :: Action
flood = Action ForwardFlood []

forward :: Port -> Action
forward p = Action (ForwardPorts (Set.singleton p)) []