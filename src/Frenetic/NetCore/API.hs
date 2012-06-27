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
  ( Predicate (..)
  , Policy (..)
  , interpretPolicy
  ) where

import           Frenetic.Compat
import Frenetic.NetCore.Action
import qualified Data.List       as List
import           Data.Bits
import           Data.Word
import qualified Data.Set        as Set

{-| Predicates denote sets of (switch, packet) pairs. -}
data Predicate = PrPattern Pattern
               | PrTo Switch 
               | PrUnion Predicate Predicate
               | PrIntersect Predicate Predicate
               | PrNegate Predicate

{-| Policies denote functions from (switch, packet) to packets. -}
data Policy = PoBasic Predicate Action
            | PoUnion Policy Policy
            | PoIntersect Policy Policy
              
instance Show Predicate where
  show (PrPattern pat) = show pat  
  show (PrTo s) = "switch(" ++ show s ++ ")"
  show (PrUnion pr1 pr2) = "(" ++ show pr1 ++ ") \\/ (" ++ show pr2 ++ ")"
  show (PrIntersect pr1 pr2) = "(" ++ show pr1 ++ ") /\\ (" ++ show pr2 ++ ")"
  show (PrNegate pr) = "~(" ++ show pr ++ ")"
              
instance Show Policy where
  show (PoBasic pr as) = "(" ++ show pr ++ ") -> " ++ show as
  show (PoUnion po1 po2) = "(" ++ show po1 ++ ") \\/ (" ++ show po2 ++ ")"
  show (PoIntersect po1 po2) = "(" ++ show po1 ++ ") /\\ (" ++ show po2 ++ ")"

-- |Implements the denotation function for predicates.
interpretPredicate :: FreneticImpl a
                   => Predicate
                   -> Transmission (PatternImpl a) (PacketImpl a)
                   -> Bool
interpretPredicate (PrPattern ptrn) tr = 
  FreneticPkt (toPacket (trPkt tr)) `ptrnMatchPkt` FreneticPat ptrn
interpretPredicate (PrTo sw) tr =
  sw == trSwitch tr
interpretPredicate (PrUnion pr1 pr2) tr = 
  interpretPredicate pr1 tr || interpretPredicate pr2 tr
interpretPredicate (PrIntersect pr1 pr2) tr = 
   interpretPredicate pr1 tr && interpretPredicate pr2 tr
interpretPredicate (PrNegate pr) tr =
  not (interpretPredicate pr tr)

-- |Implements the denotation function for policies.
interpretPolicy :: FreneticImpl a
                => Policy
                -> Transmission (PatternImpl a) (PacketImpl a)
                -> Action
interpretPolicy (PoBasic pred acts) tr = case interpretPredicate pred tr of
  True -> acts 
  False -> emptyAction
interpretPolicy (PoUnion p1 p2) tr = 
  interpretPolicy p1 tr `unionAction` interpretPolicy p2 tr
interpretPolicy (PoIntersect p1 p2) tr = 
  interpretPolicy p1 tr `interAction` interpretPolicy p2 tr
