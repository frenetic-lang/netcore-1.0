--------------------------------------------------------------------------------
-- The Frenetic Project                                                       --
-- frenetic@frenetic-lang.org                                                 --
--------------------------------------------------------------------------------
-- Licensed to the Frenetic Project by one or more contributors. See the      --
-- NOTICE file distributed with this work for additional information          --
-- regarding copyright and ownership. The Frenetic Project licenses this      --
-- file to you under the following license.                                   --
--                                                                            --
-- Redistribution and use in source and binary forms, with or without         --
-- modification, are permitted provided the following conditions are met:     --
-- - Redistributions of source code must retain the above copyright           --
--   notice, this list of conditions and the following disclaimer.            --
-- - Redistributions of binaries must reproduce the above copyright           --
--   notice, this list of conditions and the following disclaimer in          --
--   the documentation or other materials provided with the distribution.     --
-- - The names of the copyright holds and contributors may not be used to     --
--   endorse or promote products derived from this work without specific      --
--   prior written permission.                                                --
--                                                                            --
-- Unless required by applicable law or agreed to in writing, software        --
-- distributed under the License is distributed on an "AS IS" BASIS, WITHOUT  --
-- WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the   --
-- LICENSE file distributed with this work for specific language governing    --
-- permissions and limitations under the License.                             --
--------------------------------------------------------------------------------
-- /src/Frenetic/NetCore/API.hs                                               --
--------------------------------------------------------------------------------

module Frenetic.NetCore.API
  ( -- * Basic types
    Switch
  , Port
  , PseudoPort (..)
  , Word48
  -- * Actions
  , Action (..)
  , Forward (..)
  , NumPktQuery
  -- ** Basic actions
  , flood
  , forward
  , query
  , dropPkt
  -- ** Action composition
  , unionAction
  , interAction
  -- ** Inspecting actions
  , actionForwardsTo
  -- * Patterns
  , Pattern (..)
  -- * Predicates
  , Predicate (..)
  -- ** Predicate composition
  , prDifference
  , prNaryUnion
  -- * Policies
  , Policy (..)
  -- ** Policy composition
  , poRestrict
  , poNaryUnion
  ) where

import Frenetic.Util
import qualified Data.List as List
import Data.Bits
import Data.Word
import qualified Data.Set as Set
import qualified Data.Map as Map
import Frenetic.LargeWord
import Frenetic.Pattern

{-| The type of switches in the network. -}
type Switch = Word64

{-| The type of switch ports. -}
type Port = Word16

{-| The type of logical switch ports. -}
data PseudoPort = Physical Port | PhysicalFlood deriving (Ord, Eq, Show)

{-| Auxillary value for ethernet addresses.  -}
type Word48 = LargeKey Word8 (LargeKey Word8 (LargeKey Word8 (LargeKey Word8 (LargeKey Word8 Word8))))

-- |Frenetic patterns
data Pattern = Pattern {
  ptrnDlSrc :: Wildcard Word48
  , ptrnDlDst :: Wildcard Word48
  , ptrnDlTyp :: Wildcard Word16
  , ptrnDlVlan :: Wildcard Word16
  , ptrnDlVlanPcp :: Wildcard Word8
  , ptrnNwSrc :: Wildcard Word32
  , ptrnNwDst :: Wildcard Word32
  , ptrnNwProto :: Wildcard Word8
  , ptrnNwTos :: Wildcard Word8
  , ptrnTpSrc :: Wildcard Word16
  , ptrnTpDst :: Wildcard Word16
  , ptrnInPort :: Maybe Port
  } deriving (Ord, Show, Eq)

instance Matchable Pattern where
  top = Pattern {
    ptrnDlSrc = top
    , ptrnDlDst = top
    , ptrnDlTyp = top
    , ptrnDlVlan = top
    , ptrnDlVlanPcp = top
    , ptrnNwSrc = top
    , ptrnNwDst = top
    , ptrnNwProto = top
    , ptrnNwTos = top
    , ptrnTpSrc = top
    , ptrnTpDst = top
    , ptrnInPort = top
    }

  intersect p1 p2 = do ptrnDlSrc' <- intersect (ptrnDlSrc p1) (ptrnDlSrc p2)
                       ptrnDlDst' <- intersect (ptrnDlDst p1) (ptrnDlDst p2)
                       ptrnDlTyp' <- intersect (ptrnDlTyp p1) (ptrnDlTyp p2)
                       ptrnDlVlan' <- intersect (ptrnDlVlan p1) (ptrnDlVlan p2)
                       ptrnDlVlanPcp' <- intersect (ptrnDlVlanPcp p1) (ptrnDlVlanPcp p2)
                       ptrnNwSrc' <- intersect (ptrnNwSrc p1) (ptrnNwSrc p2)
                       ptrnNwDst' <- intersect (ptrnNwDst p1) (ptrnNwDst p2)
                       ptrnNwProto' <- intersect (ptrnNwProto p1) (ptrnNwProto p2)
                       ptrnNwTos' <- intersect (ptrnNwTos p1) (ptrnNwTos p2)
                       ptrnTpSrc' <- intersect (ptrnTpSrc p1) (ptrnTpSrc p2)
                       ptrnTpDst' <- intersect (ptrnTpDst p1) (ptrnTpDst p2)
                       ptrnInPort' <- intersect (ptrnInPort p1) (ptrnInPort p2)
                       return Pattern {
                         ptrnDlSrc = ptrnDlSrc'
                         , ptrnDlDst = ptrnDlDst'
                         , ptrnDlTyp = ptrnDlTyp'
                         , ptrnDlVlan = ptrnDlVlan'
                         , ptrnDlVlanPcp = ptrnDlVlanPcp'
                         , ptrnNwSrc = ptrnNwSrc'
                         , ptrnNwDst = ptrnNwDst'
                         , ptrnNwProto = ptrnNwProto'
                         , ptrnNwTos = ptrnNwTos'
                         , ptrnTpSrc = ptrnTpSrc'
                         , ptrnTpDst = ptrnTpDst'
                         , ptrnInPort = ptrnInPort'
                         }

type Rewrites = Set Pattern

-- | Forward represents a set of forwards with modifications on a switch,
-- including forwarding multiple packets, so long as they are *different*
-- packets (i.e., have a different modification applied to each).  Flood is
-- encoded by using the PseudoPort Flood rather than
data Forward = ForwardPorts (Map PseudoPort Rewrites)
  deriving (Eq)

instance Show Forward where
  show (ForwardPorts m) = show (Map.keys m)

type NumPktQuery = (Chan (Switch, Integer), Int)

data Action = Action {
  actionForwards :: Forward,
  actionNumPktQueries :: [NumPktQuery]
} deriving (Eq)

actionForwardsTo :: Action
                 -> Set PseudoPort -- ^'Nothing' indicates flood
actionForwardsTo (Action (ForwardPorts m) _) = (Set.fromList . Map.keys $ m)

instance Show Action where
  show (Action fwd _) = "<fwd=" ++ show fwd ++ ">"

dropPkt :: Action
dropPkt = Action (ForwardPorts Map.empty) []

unionForward :: Forward -> Forward -> Forward
unionForward (ForwardPorts m1) (ForwardPorts m2) =
  ForwardPorts (Map.unionWith Set.union m1 m2)

interForward :: Forward -> Forward -> Forward
interForward (ForwardPorts m1) (ForwardPorts m2) =
  ForwardPorts (Map.intersectionWith Set.intersection m1 m2)

unionAction :: Action -> Action -> Action
unionAction (Action fwd1 q1) (Action fwd2 q2) =
  Action (unionForward fwd1 fwd2) (unionQuery q1 q2)
    where unionQuery xs ys = xs ++ filter (\y -> not (y `elem` xs)) ys

interAction :: Action -> Action -> Action
interAction (Action fwd1 q1) (Action fwd2 q2) =
  Action (interForward fwd1 fwd2) (interQuery q1 q2)
    where interQuery xs ys = filter (\x -> x `elem` ys) xs

flood :: Action
flood = Action (ForwardPorts (Map.singleton PhysicalFlood
                                            (Set.singleton top))) []

forward :: Port -> Action
forward p = Action (ForwardPorts (Map.singleton (Physical p)
                                                (Set.singleton top))) []

query :: Int -> IO (Chan (Switch, Integer), Action)
query millisecondInterval = do
  ch <- newChan
  return (ch, Action (ForwardPorts Map.empty) [(ch, millisecondInterval)])

-- |Construct the set difference between p1 and p2
prDifference :: Predicate -> Predicate -> Predicate
prDifference p1 p2 = PrIntersect p1 (PrNegate p2)

prNaryUnion :: [Predicate] -> Predicate
prNaryUnion [] = PrNegate top
prNaryUnion ps = List.foldr1 (\ p1 p2 -> PrUnion p1 p2) ps

-- |Construct the policy restricted by the predicate
poRestrict :: Policy -> Predicate -> Policy
poRestrict policy pred=
  case policy of
    PoBottom -> PoBottom
    PoBasic predicate act -> PoBasic (PrIntersect predicate pred) act
    PoUnion p1 p2 -> PoUnion (poRestrict p1 pred) (poRestrict p2 pred)
    PoIntersect p1 p2 -> PoIntersect (poRestrict p1 pred) (poRestrict p2 pred)

poNaryUnion :: [Policy] -> Policy
poNaryUnion [] = PoBottom
poNaryUnion ps = List.foldr1 (\ p1 p2 -> PoUnion p1 p2) ps

{-| Predicates denote sets of (switch, packet) pairs. -}
data Predicate = PrPattern Pattern
               | PrTo Switch
               | PrUnion Predicate Predicate
               | PrIntersect Predicate Predicate
               | PrNegate Predicate
  deriving (Eq)

{-| Policies denote functions from (switch, packet) to packets. -}
data Policy = PoBottom
            | PoBasic Predicate Action
            | PoUnion Policy Policy
            | PoIntersect Policy Policy

instance Show Predicate where
  show (PrPattern pat) = show pat
  show (PrTo s) = "switch(" ++ show s ++ ")"
  show (PrUnion pr1 pr2) = "(" ++ show pr1 ++ ") \\/ (" ++ show pr2 ++ ")"
  show (PrIntersect pr1 pr2) = "(" ++ show pr1 ++ ") /\\ (" ++ show pr2 ++ ")"
  show (PrNegate pr) = "~(" ++ show pr ++ ")"

instance Show Policy where
  show PoBottom = "(PoBottom)"
  show (PoBasic pr as) = "(" ++ show pr ++ ") -> " ++ show as
  show (PoUnion po1 po2) = "(" ++ show po1 ++ ") \\/ (" ++ show po2 ++ ")"
  show (PoIntersect po1 po2) = "(" ++ show po1 ++ ") /\\ (" ++ show po2 ++ ")"

instance Matchable Predicate where
  top = PrPattern top
  intersect p1 p2 = Just (PrIntersect p1 p2)
