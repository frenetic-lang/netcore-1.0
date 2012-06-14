module Frenetic.NetCore.Action
  ( Port
  , Forward (..)
  , Action (..)
  , GAction (..)
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
  actionFwd :: Forward
} deriving (Eq)

instance Show Action where
  show (Action fwd) = "<fwd=" ++ show fwd ++ ">"

emptyAction :: Action
emptyAction = Action (ForwardPorts Set.empty)

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
unionAction (Action fwd1) (Action fwd2) = Action (unionForward fwd1 fwd2)

interAction :: Action -> Action -> Action
interAction (Action fwd1) (Action fwd2) = Action (interForward fwd1 fwd2)

-- |This class represents backend actions.
class (Show actn, Eq actn) => GAction actn where
  actnDefault :: actn
  actnController :: actn
  actnTranslate :: Action -> actn

flood :: Action
flood = Action ForwardFlood

forward :: Port -> Action
forward p = Action (ForwardPorts (Set.singleton p))