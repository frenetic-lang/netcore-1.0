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
-- /src/Frenetic/NetCore/API.hs                                               --
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
    ScopedTypeVariables,
    DeriveDataTypeable
 #-}


module Frenetic.NetCore.API 
  ( Predicate (..)
  , Policy (..)
  , interpretPolicy
  ) where

import           Frenetic.Compat

import qualified Data.List       as List
import           Data.Bits

import           Data.Word
import qualified Data.Set        as Set
import           Data.Typeable
import           Data.Dynamic


{-| Predicates denote sets of (switch, packet) pairs. -}
data Predicate = PrPattern Pattern
               | PrUnknown
               | PrSwitchPattern String Dynamic
               | PrTo Switch 
               | PrUnion Predicate Predicate
               | PrIntersect Predicate Predicate
               | PrDifference Predicate Predicate
               | PrNegate Predicate

{-| Policies denote functions from (switch, packet) to packets. -}
data Policy = PoBasic Predicate Actions
            | PoUnknown 
            | PoUnion Policy Policy
            | PoIntersect Policy Policy
            | PoDifference Policy Policy
              
              
instance Show Predicate where
  show (PrPattern pat) = show pat  
  show (PrUnknown) = "???"
  show (PrTo s) = "switch(" ++ show s ++ ")"
  show (PrSwitchPattern desc _) = desc
  show (PrUnion pr1 pr2) = "(" ++ show pr1 ++ ") \\/ (" ++ show pr2 ++ ")"
  show (PrIntersect pr1 pr2) = "(" ++ show pr1 ++ ") /\\ (" ++ show pr2 ++ ")"
  show (PrDifference pr1 pr2) = "(" ++ show pr1 ++ ") // (" ++ show pr2 ++ ")"
  show (PrNegate pr) = "~(" ++ show pr ++ ")"
              
instance Show Policy where
  show (PoBasic pr as) = "(" ++ show pr ++ ") -> " ++ show as
  show (PoUnknown) = "???"
  show (PoUnion po1 po2) = "(" ++ show po1 ++ ") \\/ (" ++ show po2 ++ ")"
  show (PoIntersect po1 po2) = "(" ++ show po1 ++ ") /\\ (" ++ show po2 ++ ")"
  show (PoDifference po1 po2) = "(" ++ show po1 ++ ") \\\\ (" ++ show po2 ++ ")"


{-| Implements the denotation function for predicates. -}
interpretPredicate :: forall ptrn pkt. (ValidTransmission ptrn pkt) =>
                      Predicate
                   -> Transmission ptrn pkt
                   -> (Bool, Bool)
interpretPredicate (PrPattern ptrn) tr = 
    let rv = toPacket (trPkt tr) `ptrnMatchPkt` ptrn in 
      (rv, rv)
interpretPredicate (PrSwitchPattern _ dyn) tr =
    case fromDynamic dyn :: Maybe ptrn of
      Just ptrn -> let k = ptrnMatchPkt (trPkt tr) ptrn in (k, k)
      Nothing -> (False, False)
interpretPredicate (PrUnknown) tr = (False, True) 
interpretPredicate (PrTo sw) tr = 
    let rv = sw == trSwitch tr in
      (rv, rv)
interpretPredicate (PrUnion pr1 pr2) t = 
    let (b1, b1') = interpretPredicate pr1 t in
    let (b2, b2') = interpretPredicate pr2 t in
      (b1 || b2, b1' || b2)
interpretPredicate (PrIntersect pr1 pr2) t = 
    let (b1, b1') = interpretPredicate pr1 t in
    let (b2, b2') = interpretPredicate pr2 t in
      (b1 && b2, b1' && b2)
interpretPredicate (PrDifference pr1 pr2) t = 
    let (b1, b1') = interpretPredicate pr1 t in
    let (b2, b2') = interpretPredicate pr2 t in
      (b1 && not b2, b1' && not b2)
interpretPredicate (PrNegate pr) t =
    let (b1, b1') = interpretPredicate pr t in
      (not b1, not b1')
interpretPredicate p t = (False, False)

-- {-| Implements the denotation function for actions. -}
-- interpretActions :: (GPacket pkt) => pkt -> Actions -> Set.Set pkt
-- interpretActions pkt actn = Set.fromList [updatePacket pkt ((toPacket pkt) { pktInPort = prt' }) 
--                                          | prt' <- Set.toList actn] 

{-| Implements the denotation function for policies. -}
interpretPolicy :: (ValidTransmission ptrn pkt) =>
                   Policy
                -> Transmission ptrn pkt
                -> (Actions, Actions)
interpretPolicy (PoBasic pred as) tr = case interpretPredicate pred tr of
    (True, True) -> (as, as)
    (True, False) -> (as, Set.empty)
    (False, True) -> (Set.empty, as)
    (False, False) -> (Set.empty, Set.empty)
-- TODO: NYI
-- interpretPolicy PoUnknown tr = (Set.empty, undefined)
interpretPolicy (PoUnion p1 p2) tr = 
  pairLift Set.union (interpretPolicy p1 tr) (interpretPolicy p2 tr)
interpretPolicy (PoIntersect p1 p2) tr = 
  pairLift Set.intersection (interpretPolicy p1 tr) (interpretPolicy p2 tr)
interpretPolicy (PoDifference p1 p2) tr = 
  pairLift Set.difference (interpretPolicy p1 tr) (interpretPolicy p2 tr)

pairLift :: (a -> a -> a) -> (a, a) -> (a, a) -> (a, a)
pairLift f (a1, a1') (a2, a2') = (f a1 a2, f a1' a2')
