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
-- /src/Frenetic/Compat.hs                                                    --
--------------------------------------------------------------------------------

module Frenetic.Compat
  ( Switch
  , Port
  , Word48
  , Packet (..)
  , Pattern (..)
  , Transmission (..)
  -- * Actions
  , Action
  , NumPktQuery
  -- ** Basic actions
  , flood
  , forward
  , query
  , emptyAction
  -- ** Action composition
  , unionAction
  , interAction
  -- ** Inspecting actions
  , actionNumPktQueries
  , actionForwardsTo
  -- * Implementation
  , FreneticImpl (..)
  )  where

import Frenetic.Util
import qualified Data.List          as List
import           Data.Bits
import           Data.Word
import qualified Data.Set           as Set
import           Frenetic.Pattern
import           Frenetic.LargeWord

{-| The type of switches in the network. -}
type Switch = Word64

{-| The type of switch ports. -}
type Port = Word16

{-| Auxillary value for ethernet addresses.  -}
type Word48 = LargeKey Word8 (LargeKey Word8 (LargeKey Word8 (LargeKey Word8 (LargeKey Word8 Word8))))

-- |Frenetic packets
data Packet = Packet {
    pktDlSrc :: Word48
  , pktDlDst :: Word48
  , pktDlTyp :: Word16
  , pktDlVlan :: Word16
  , pktDlVlanPcp :: Word8
  , pktNwSrc :: Word32
  , pktNwDst :: Word32
  , pktNwProto :: Word8
  , pktNwTos :: Word8
  , pktTpSrc :: Word16
  , pktTpDst :: Word16
  , pktInPort :: Port
  } deriving (Show, Eq, Ord)
             
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
  } deriving (Show, Eq)

{-| Data that was sent. -}
data Transmission ptrn pkt = Transmission {
      trPattern :: ptrn,
      trSwitch :: Switch,
      trPkt :: pkt
    } deriving (Eq)

data Forward
  = ForwardPorts (Set Port)
  | ForwardFlood 
  deriving (Eq)

instance Show Forward where
  show (ForwardPorts set) = show (Set.toList set)
  show ForwardFlood = "Flood"

type NumPktQuery = (Chan Integer, Int)

data Action = Action {
  actionForwards :: Forward,
  actionNumPktQueries :: [NumPktQuery]
} deriving (Eq)

actionForwardsTo :: Action 
                 -> Maybe (Set Port) -- ^'Nothing' indicates flood
actionForwardsTo (Action (ForwardPorts set) _) = Just set
actionForwardsTo (Action ForwardFlood _) = Nothing

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

query :: Int -> IO Action
query millisecondInterval = do
  ch <- newChan
  return (Action (ForwardPorts Set.empty) [(ch, millisecondInterval)])

-- |'FreneticImpl a' is a family of related abstract types that define a
-- back-end for Frenetic. 
class (Show (PatternImpl a), 
       Show (ActionImpl a),
       Matchable (PatternImpl a), 
       Eq (PacketImpl a),
       Eq (ActionImpl a), 
       Eq (PatternImpl a))
       => FreneticImpl a where

  data PacketImpl a
  -- |'PatternImpl a' represents switch-level patterns, which may not be
  -- as expressive as Frenetic's pattern language.
  --
  --  @patOverapprox@ and @patUnderapprox@ must follow the laws in the
  -- Approx class. If the pattern is not a real underapproximation,
  -- @patUnderapprox@ must return Nothing.
  data PatternImpl a
  -- |'ActionImpl a' represents switch-level actions. All Frenetic actions
  -- (@Action@) may not be realizable on switches.
  data ActionImpl a
  
  -- |'ptrnMatchPkt pkt pat' is 'True' if 'pat' matches 'pkt'.
  ptrnMatchPkt :: PacketImpl a -> PatternImpl a -> Bool
  toPacket :: PacketImpl a -> Packet

  updatePacket :: PacketImpl a -> Packet -> PacketImpl a  

  fromPatternOverapprox :: Pattern -> PatternImpl a
  fromPatternUnderapprox :: Packet -> Pattern -> Maybe (PatternImpl a)
  toPattern :: PatternImpl a -> Pattern

  actnDefault :: ActionImpl a 
  actnController :: ActionImpl a
  actnTranslate :: Action -> ActionImpl a

