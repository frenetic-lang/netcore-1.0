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
-- * Redistributions of source code must retain the above copyright           --
--   notice, this list of conditions and the following disclaimer.            --
-- * Redistributions of binaries must reproduce the above copyright           --
--   notice, this list of conditions and the following disclaimer in          --
--   the documentation or other materials provided with the distribution.     --
-- * The names of the copyright holds and contributors may not be used to     --
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
--                                                                            --
-- $Id$ --
--------------------------------------------------------------------------------

{-# LANGUAGE
    NoMonomorphismRestriction,
    StandaloneDeriving,
    FlexibleInstances,
    Rank2Types,
    GADTs,
    ExistentialQuantification,
    MultiParamTypeClasses,
    FunctionalDependencies,
    ScopedTypeVariables
 #-}

module Frenetic.Compat where

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
{-| Frenetic "packets" -}
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
             
{-| Frenetic "patterns" -}
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

{-| Generic packets -}
class (Show pkt, Ord pkt, Eq pkt) => GPacket pkt where
  toPacket :: pkt -> Packet
  updatePacket :: pkt -> Packet -> pkt

{-|
This class represents backend patterns.

* @patOverapprox@ and @patUnderapprox@ must follow the laws in the
  Approx class. If the pattern is not a real underapproximation,
  @patUnderapprox@ must return Nothing.
-}
class (Show ptrn, Matchable ptrn) => GPattern ptrn where
    fromPatternOverapprox :: Pattern -> ptrn
    fromPatternUnderapprox :: Packet -> Pattern -> Maybe ptrn
    toPattern :: ptrn -> Pattern
  
{-| A valid transmission has a matching relationship between the pattern and packet -}
class (GPattern ptrn, GPacket pkt) => ValidTransmission ptrn pkt where
     ptrnMatchPkt :: pkt -> ptrn -> Bool

instance GPacket Packet where 
  toPacket = id
  updatePacket pkt1 pkt2 = pkt2

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
                         
instance GPattern Pattern where
  fromPatternOverapprox = id
  fromPatternUnderapprox pkt ptrn = Nothing -- We never need to underapproximate real patterns
  toPattern = id

instance ValidTransmission Pattern Packet where
  ptrnMatchPkt pkt ptrn = wMatch (pktDlSrc pkt) (ptrnDlSrc ptrn)
                          && wMatch (pktDlDst pkt) (ptrnDlDst ptrn)
                          && wMatch (pktDlTyp pkt) (ptrnDlTyp ptrn)
                          && wMatch (pktDlVlan pkt) (ptrnDlVlan ptrn)
                          && wMatch (pktDlVlanPcp pkt) (ptrnDlVlanPcp ptrn)
                          && wMatch (pktNwSrc pkt) (ptrnNwSrc ptrn)
                          && wMatch (pktNwDst pkt) (ptrnNwDst ptrn)
                          && wMatch (pktNwProto pkt) (ptrnNwProto ptrn)
                          && wMatch (pktNwTos pkt) (ptrnNwTos ptrn)
                          && wMatch (pktTpSrc pkt) (ptrnTpSrc ptrn)
                          && wMatch (pktTpDst pkt) (ptrnTpDst ptrn)
                          && Just (pktInPort pkt) `match` ptrnInPort ptrn  
