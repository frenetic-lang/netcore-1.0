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
-- /src/Language.hs                                                           --
-- Frenetic Language stuff                                                    --
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
    ScopedTypeVariables,
DeriveDataTypeable
 #-}

module Frenetic.Language where

import qualified Data.List          as List
import           Data.Bits

import           Data.Word
import qualified Data.Set           as Set
import           Data.Typeable
import           Data.Dynamic

import           Frenetic.Pattern
import           Frenetic.LargeWord


--
-- Basic network elements
--

type Word48 = LargeKey Word8 (LargeKey Word8 (LargeKey Word8 (LargeKey Word8 (LargeKey Word8 Word8))))

type Switch = Word64
type Port = Word16


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
  } deriving (Show, Eq, Typeable)

{-| Generic packets -}
class (Show pkt, Eq pkt) => GPacket pkt where
  toPacket :: pkt -> Packet
  updatePacket :: pkt -> Packet -> pkt
 
instance GPacket Packet where 
  toPacket = id
  updatePacket pkt1 pkt2 = pkt2

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
  } deriving (Show, Eq, Typeable)
                    
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
                       return $ Pattern {
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

{-|
This class represents backend patterns.

* @patOverapprox@ and @patUnderapprox@ must follow the laws in the
  Approx class. If the pattern is not a real underapproximation,
  @patUnderapprox@ must return Nothing.
-}
class (Typeable ptrn, Show ptrn, Matchable ptrn) => GPattern ptrn where
    fromPatternOverapprox :: Pattern -> ptrn
    fromPatternUnderapprox :: Packet -> Pattern -> Maybe ptrn
    toPattern :: ptrn -> Pattern

instance GPattern Pattern where
  fromPatternOverapprox = id
  fromPatternUnderapprox pkt ptrn = Nothing -- We never need to underapproximate real patterns
  toPattern = id
  
{-| Something sent. See below relation -}
data Transmission ptrn pkt = Transmission {
      trPattern :: ptrn,
      trSwitch :: Switch,
      trPkt :: pkt
    } deriving (Eq)

class (GPattern ptrn, GPacket pkt) => ValidTransmission ptrn pkt where
     ptrnMatchPkt :: pkt -> ptrn -> Bool

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
                          && Just (pktInPort pkt) `match` (ptrnInPort ptrn)

  

{-| This class represents backend actions. |-}
class (Show actn, Eq actn) => GAction actn where
    actnDefault :: actn
    actnController :: actn
    actnTranslate :: Actions -> actn
                
--
-- Predicates
--

-- data HeaderW = forall r. (Show r, Bits r) => HeaderW (Header r)

-- -- Can't derive these.
-- instance Show HeaderW where
--     show (HeaderW h) = show h
                       
-- type Headers = [HeaderW]

-- data Inspector = Inspector {
--       insDesc :: String,
--       insApply :: (forall ptrn pkt. ValidTransmission ptrn pkt => Transmission ptrn pkt -> Bool),
--       insInv :: (forall ptrn pkt. ValidTransmission ptrn pkt => Transmission ptrn pkt -> Maybe Headers)
--     }

-- data Doer = Doer {
--       doDesc :: String,
--       doApply :: (forall ptrn pkt. ValidTransmission ptrn pkt =>
--                   Transmission ptrn pkt ->
--                   [Transmission ptrn pkt]),
--       doInv :: (forall ptrn pkt. ValidTransmission ptrn pkt => Transmission ptrn pkt -> Maybe Headers)
--     }


data Predicate = PrPattern Pattern
               | PrSwitchPattern String Dynamic
--               | PrInspect Inspector
               | PrTo Switch 
               | PrUnion Predicate Predicate
               | PrIntersect Predicate Predicate
               | PrDifference Predicate Predicate
               | PrNegate Predicate

instance Show Predicate where
-- FIX
  show (PrPattern pat) = show pat  
  show (PrTo s) = "switch(" ++ show s ++ ")"
--  show (PrInspect ins) = insDesc ins
  show (PrSwitchPattern desc _) = desc
  show (PrUnion pr1 pr2) = "(" ++ show pr1 ++ ") \\/ (" ++ show pr2 ++ ")"
  show (PrIntersect pr1 pr2) = "(" ++ show pr1 ++ ") /\\ (" ++ show pr2 ++ ")"
  show (PrDifference pr1 pr2) = "(" ++ show pr1 ++ ") // (" ++ show pr2 ++ ")"
  show (PrNegate pr) = "~(" ++ show pr ++ ")"

-- 
-- Policies
--

type Actions = Set.Set Port

data Policy = PoBasic Predicate Actions
--            | PoDoer Doer
            | PoUnion Policy Policy
            | PoIntersect Policy Policy
            | PoDifference Policy Policy
                  
instance Show Policy where
  show (PoBasic pr as) = "(" ++ show pr ++ ") -> " ++ show as
--  show (PoDoer doer) = doDesc doer 
  show (PoUnion po1 po2) = "(" ++ show po1 ++ ") \\/ (" ++ show po2 ++ ")"
  show (PoIntersect po1 po2) = "(" ++ show po1 ++ ") /\\ (" ++ show po2 ++ ")"
  show (PoDifference po1 po2) = "(" ++ show po1 ++ ") \\\\ (" ++ show po2 ++ ")"

--
-- Interpreter
--

interpretPredicate :: forall ptrn pkt. (ValidTransmission ptrn pkt) =>
                      Predicate
                   -> Transmission ptrn pkt
                   -> Bool
interpretPredicate (PrPattern ptrn) tr = toPacket ( trPkt tr) `ptrnMatchPkt` ptrn
interpretPredicate (PrSwitchPattern _ dyn) tr =
    case fromDynamic dyn :: Maybe ptrn of
      Just ptrn -> ptrnMatchPkt (trPkt tr) ptrn 
      Nothing -> False
--interpretPredicate (PrInspect ins) tr = insApply ins tr
interpretPredicate (PrUnion pr1 pr2) t = 
  interpretPredicate pr1 t || interpretPredicate pr2 t
interpretPredicate (PrIntersect pr1 pr2) t = 
  interpretPredicate pr1 t && interpretPredicate pr2 t
interpretPredicate (PrDifference pr1 pr2) t = 
  interpretPredicate pr1 t && not (interpretPredicate pr2 t)
interpretPredicate (PrNegate pr) t = not (interpretPredicate pr t)

interpretActions :: (GPacket pkt) => pkt -> Actions -> [pkt]
interpretActions pkt actn =
  [updatePacket pkt ((toPacket pkt) { pktInPort = prt' }) | prt' <- Set.toList actn] -- not totally sure of this FIX

-- FIX maybe we shouldn't use list set operations here.
interpretPolicy :: (ValidTransmission ptrn pkt) =>
                   Policy
                -> Transmission ptrn pkt
                -> [pkt]
interpretPolicy (PoBasic pred as) tr | interpretPredicate pred tr = interpretActions (trPkt tr) as
                                     | otherwise = []
--interpretPolicy (PoDoer doer) tr = doApply doer tr
interpretPolicy (PoUnion p1 p2) tr = 
  interpretPolicy p1 tr `List.union` interpretPolicy p2 tr
interpretPolicy (PoIntersect p1 p2) tr = 
  interpretPolicy p1 tr `List.intersect` interpretPolicy p2 tr
interpretPolicy (PoDifference p1 p2) tr = 
  (interpretPolicy p1 tr) List.\\ (interpretPolicy p2 tr)
