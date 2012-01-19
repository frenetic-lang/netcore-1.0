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

import qualified Data.List as List
import Data.Bits
import Data.LargeWord
import Data.Word
import Data.Set as Set
import Data.Typeable
import Data.Dynamic

import Frenetic.Pattern as P

--
-- Basic network elements
--

type Switch = Word64
type Port = Word16


    
type Word48 = LargeKey Word8 (LargeKey Word8 (LargeKey Word8 (LargeKey Word8 (LargeKey Word8 Word8))))

data Header b where
  Dl_src :: Header Word48
  Dl_dst :: Header Word48
  Dl_typ :: Header Word16
  Dl_vlan :: Header Word16
  Dl_vlan_pcp :: Header Word8
  Nw_src :: Header Word32
  Nw_dst :: Header Word32
  Nw_proto :: Header Word8
  Nw_tos :: Header Word8
  Tp_src :: Header Word16
  Tp_dst :: Header Word16

deriving instance Eq b => Eq (Header b)
deriving instance Show b => Show (Header b)
deriving instance Ord b => Ord (Header b)

class (Eq pkt) => Packet pkt where
  pktGetHeader :: (Bits b) => pkt -> Header b -> b
  pktSetHeader :: (Bits b) => pkt -> Header b -> b -> pkt

-- remember this used to be existential.
-- more elegant way to store ptrn later.
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
class (P.Pattern ptrn) => Patternable ptrn where
    patOverapprox :: Header r -> P.Wildcard r -> ptrn
    patInport :: Port -> ptrn
    patUnderapprox :: Header r -> P.Wildcard r -> r -> Maybe ptrn

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

data HeaderW = forall r. (Bits r) => HeaderW (Header r)

-- Can't derive these.
instance Show HeaderW where
    show (HeaderW h) = show h
                       
type Headers = [HeaderW]

data Inspector = Inspector {
      insDesc :: String,
      insApply :: (forall ptrn pkt. Transmissionable ptrn pkt => Transmission ptrn pkt -> Bool),
      insInv :: (forall ptrn pkt. Transmissionable ptrn pkt => Transmission ptrn pkt -> Maybe Headers)
    }

data Doer = Doer {
      doDesc :: String,
      doApply :: (forall ptrn pkt. Transmissionable ptrn pkt =>
                  Transmission ptrn pkt ->
                  [Transmission ptrn pkt]),
      doInv :: (forall ptrn pkt. Transmissionable ptrn pkt => Transmission ptrn pkt -> Maybe Headers)
    }


data Predicate = forall b. (Bits b) => PrHeader (Header b) (P.Wildcard b)
               | PrPattern String Dynamic
               | PrInspect Inspector
               | PrTo Switch
               | PrInport Port
               | PrUnion Predicate Predicate
               | PrIntersect Predicate Predicate
               | PrDifference Predicate Predicate
               | PrNegate Predicate

instance Show Predicate where
  show (PrHeader h w) = "(" ++ show h ++ " : " ++ show w ++ ")"
  show (PrTo s) = "switch(" ++ show s ++ ")"
  show (PrInspect ins) = insDesc ins
  show (PrPattern desc _) = desc
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
            | PoDoer Doer
            | PoUnion Policy Policy
            | PoIntersect Policy Policy
            | PoDifference Policy Policy
                  
instance Show Policy where
  show (PoBasic pr as) = "(" ++ show pr ++ ") -> " ++ show as
  show (PoDoer doer) = doDesc doer 
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
interpretPredicate (PrHeader h w) tr = wBitsMake (pktGetHeader (trPkt tr) h) == w
interpretPredicate (PrPattern _ dyn) tr =
    case fromDynamic dyn :: Maybe ptrn of
      Just ptrn -> patMatch ptrn (trPkt tr)
      Nothing -> False
interpretPredicate (PrInspect ins) tr = insApply ins tr
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
interpretPolicy (PoDoer doer) tr = doApply doer tr
interpretPolicy (PoUnion p1 p2) tr = 
  interpretPolicy p1 tr `List.union` interpretPolicy p2 tr
interpretPolicy (PoIntersect p1 p2) tr = 
  interpretPolicy p1 tr `List.intersect` interpretPolicy p2 tr
interpretPolicy (PoDifference p1 p2) tr = 
  (interpretPolicy p1 tr) List.\\ (interpretPolicy p2 tr)
