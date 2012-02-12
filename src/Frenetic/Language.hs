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
    ScopedTypeVariables
 #-}

module Frenetic.Language where

import qualified Data.List        as List
import           Data.Bits
import           Data.LargeWord
import           Data.Word
import           Data.Set         as Set
import           Data.Typeable
import           Data.Dynamic

import           Frenetic.Pattern as P

--
-- Basic network elements
--

type Switch = Word64
type Port = Word16

type Word48 = LargeKey Word8 (LargeKey Word8 (LargeKey Word8 (LargeKey Word8 (LargeKey Word8 Word8))))

data IdealPacket = IdealPacket {
    ipktDlSrc :: Word48
  , ipktDlDst :: Word48
  , ipktDlTyp :: Word16
  , ipktDlVlan :: Word16
  , ipktDlVlanPcp :: Word8
  , ipktNwSrc :: Word32
  , ipktNwDst :: Word32
  , ipktNwProto :: Word8
  , ipktNwTos :: Word8
  , ipktTpSrc :: Word16
  , ipktTpDst :: Wildcard Word16
  , ipktInPort :: Port
  , ipktPayload :: String
  } deriving (Show, Eq)

data IdealPattern = IdealPattern { 
  ipatDlSrc :: Wildcard Word48
  , ipatDlDst :: Wildcard Word48
  , ipatDlTyp :: Wildcard Word16
  , ipatDlVlan :: Wildcard Word16
  , ipatDlVlanPcp :: Wildcard Word8
  , ipatNwSrc :: Wildcard Word32
  , ipatNwDst :: Wildcard Word32
  , ipatNwProto :: Wildcard Word8
  , ipatNwTos :: Wildcard Word8
  , ipatTpSrc :: Wildcard Word16
  , ipatTpDst :: Wildcard Word16
  , ipatInPort :: Maybe Port
  } deriving (Show, Eq)
                    
instance P.Pattern IdealPattern where
  top = IdealPattern {
    ipatDlSrc = P.top
    , ipatDlDst = P.top
    , ipatDlTyp = P.top
    , ipatDlVlan = P.top
    , ipatDlVlanPcp = P.top
    , ipatNwSrc = P.top
    , ipatNwDst = P.top
    , ipatNwProto = P.top
    , ipatNwTos = P.top
    , ipatTpSrc = P.top
    , ipatTpDst = P.top
    , ipatInPort = P.top
    }
        
  intersect p1 p2 = do ipatDlSrc' <- intersect (ipatDlSrc p1) (ipatDlSrc p2)
                       ipatDlDst' <- intersect (ipatDlDst p1) (ipatDlDst p2)
                       ipatDlTyp' <- intersect (ipatDlTyp p1) (ipatDlTyp p2)
                       ipatDlVlan' <- intersect (ipatDlVlan p1) (ipatDlVlan p2)
                       ipatDlVlanPcp' <- intersect (ipatDlVlanPcp p1) (ipatDlVlanPcp p2)
                       ipatNwSrc' <- intersect (ipatNwSrc p1) (ipatNwSrc p2)
                       ipatNwDst' <- intersect (ipatNwDst p1) (ipatNwDst p2)
                       ipatNwProto' <- intersect (ipatNwProto p1) (ipatNwProto p2)
                       ipatNwTos' <- intersect (ipatNwTos p1) (ipatNwTos p2)
                       ipatTpSrc' <- intersect (ipatTpSrc p1) (ipatTpSrc p2)
                       ipatTpDst' <- intersect (ipatTpDst p1) (ipatTpDst p2)
                       ipatInPort' <- intersect (ipatInPort p1) (ipatInPort p2)
                       return $ IdealPattern {
                         ipatDlSrc = ipatDlSrc'
                         , ipatDlDst = ipatDlDst'
                         , ipatDlTyp = ipatDlTyp'
                         , ipatDlVlan = ipatDlVlan'
                         , ipatDlVlanPcp = ipatDlVlanPcp'
                         , ipatNwSrc = ipatNwSrc'
                         , ipatNwDst = ipatNwDst'
                         , ipatNwProto = ipatNwProto'
                         , ipatNwTos = ipatNwTos'
                         , ipatTpSrc = ipatTpSrc'
                         , ipatTpDst = ipatTpDst'
                         , ipatInPort = ipatInPort'
                         }

instance Packet IdealPacket where 
  pktToIdeal = id
  pktFromIdeal pkt1 pkt2 = pkt2

class (Show pkt, Eq pkt) => Packet pkt where
  pktToIdeal :: pkt -> IdealPacket
  pktFromIdeal :: pkt -> IdealPacket -> pkt
  
data Transmission ptrn pkt = Transmission {
      trPattern :: ptrn,
      trSwitch :: Switch,
      trPort :: Port,
      trPkt :: pkt
    } deriving (Eq)

--
-- Core compilation classes
--

{-|
This class represents backend patterns.

* @patOverapprox@ and @patUnderapprox@ must follow the laws in the
  Approx class. If the pattern is not a real underapproximation,
  @patUnderapprox@ must return Nothing.
-}
class (Typeable ptrn, Show ptrn, P.Pattern ptrn) => Patternable ptrn where
    patOverapprox :: IdealPattern -> ptrn
    patUnderapprox :: IdealPacket -> IdealPattern -> Maybe ptrn

class (Patternable ptrn, Packet pkt) => Transmissionable ptrn pkt where
    patMatch :: ptrn -> pkt -> Bool

{-|
This class represents backend actions.
|-}
class (Eq actn) => Actionable actn where
    actDefault :: actn
    actController :: actn
    actTranslate :: Frenetic.Language.Actions -> actn
                
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
--       insApply :: (forall ptrn pkt. Transmissionable ptrn pkt => Transmission ptrn pkt -> Bool),
--       insInv :: (forall ptrn pkt. Transmissionable ptrn pkt => Transmission ptrn pkt -> Maybe Headers)
--     }

-- data Doer = Doer {
--       doDesc :: String,
--       doApply :: (forall ptrn pkt. Transmissionable ptrn pkt =>
--                   Transmission ptrn pkt ->
--                   [Transmission ptrn pkt]),
--       doInv :: (forall ptrn pkt. Transmissionable ptrn pkt => Transmission ptrn pkt -> Maybe Headers)
--     }


data Predicate = PrIdealPattern IdealPattern
               | PrSwitchPattern String Dynamic
--               | PrInspect Inspector
               | PrTo Switch
               | PrInport Port
               | PrUnion Predicate Predicate
               | PrIntersect Predicate Predicate
               | PrDifference Predicate Predicate
               | PrNegate Predicate

instance Show Predicate where
-- FIX
  show (PrIdealPattern pat) = show pat  
  show (PrTo s) = "switch(" ++ show s ++ ")"
--  show (PrInspect ins) = insDesc ins
  show (PrSwitchPattern desc _) = desc
  show (PrInport n) = "inport(" ++ show n ++ ")"
  show (PrUnion pr1 pr2) = "(" ++ show pr1 ++ ") \\/ (" ++ show pr2 ++ ")"
  show (PrIntersect pr1 pr2) = "(" ++ show pr1 ++ ") /\\ (" ++ show pr2 ++ ")"
  show (PrDifference pr1 pr2) = "(" ++ show pr1 ++ ") // (" ++ show pr2 ++ ")"
  show (PrNegate pr) = "~(" ++ show pr ++ ")"

-- 
-- Policies
--

type Actions = Set Port

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

interpretPredicate :: forall ptrn pkt. (Typeable ptrn, Transmissionable ptrn pkt) =>
                      Predicate
                   -> Transmission ptrn pkt
                   -> Bool
--interpretPredicate (PrHeader h w) tr = wBitsMake (pktGetHeader (trPkt tr) h) == w
interpretPredicate (PrSwitchPattern _ dyn) tr =
    case fromDynamic dyn :: Maybe ptrn of
      Just ptrn -> patMatch ptrn (trPkt tr)
      Nothing -> False
--interpretPredicate (PrInspect ins) tr = insApply ins tr
interpretPredicate (PrInport n) tr = n == trPort tr
interpretPredicate (PrUnion pr1 pr2) t = 
  interpretPredicate pr1 t || interpretPredicate pr2 t
interpretPredicate (PrIntersect pr1 pr2) t = 
  interpretPredicate pr1 t && interpretPredicate pr2 t
interpretPredicate (PrDifference pr1 pr2) t = 
  interpretPredicate pr1 t && not (interpretPredicate pr2 t)
interpretPredicate (PrNegate pr) t = not (interpretPredicate pr t)

interpretActions :: Transmission ptrn pkt -> Actions -> [Transmission ptrn pkt]
interpretActions (Transmission ptrn s prt pkt) actn =
    [Transmission ptrn s prt' pkt | prt' <- Set.toList actn] -- not totally sure of this FIX

-- FIX maybe we shouldn't use list set operations here.
interpretPolicy :: (Typeable ptrn, Transmissionable ptrn pkt) =>
                   Policy
                -> Transmission ptrn pkt
                -> [Transmission ptrn pkt]
interpretPolicy (PoBasic pred as) tr | interpretPredicate pred tr = interpretActions tr as
                                     | otherwise = []
--interpretPolicy (PoDoer doer) tr = doApply doer tr
interpretPolicy (PoUnion p1 p2) tr = 
  interpretPolicy p1 tr `List.union` interpretPolicy p2 tr
interpretPolicy (PoIntersect p1 p2) tr = 
  interpretPolicy p1 tr `List.intersect` interpretPolicy p2 tr
interpretPolicy (PoDifference p1 p2) tr = 
  (interpretPolicy p1 tr) List.\\ (interpretPolicy p2 tr)
