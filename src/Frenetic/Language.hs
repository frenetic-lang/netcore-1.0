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
-- /src/NetCore.hs                                                            --
-- Frenetic NetCore syntax                                                    --
-- $Id$ --
--------------------------------------------------------------------------------

{-# LANGUAGE
    NoMonomorphismRestriction,
    StandaloneDeriving,
    FlexibleInstances,
    Rank2Types,
    GADTs,
    ExistentialQuantification
 #-}

module Frenetic.Language where

import Data.Bits
import Data.Set as Set

import Frenetic.Network
import Frenetic.Pattern as P
    
--
-- Predicates
--

data Predicate p =
    forall b. (Bits b) => PrHeader (Header b) (P.Wildcard b)
  | PrTo Switch
  | PrInport Port
  | PrUnion (Predicate p) (Predicate p)
  | PrIntersect (Predicate p) (Predicate p)
  | PrDifference (Predicate p) (Predicate p)
  | PrNegate (Predicate p)

instance Show (Predicate pack) where
  show (PrHeader h bo) = "(" ++ show h ++ " : " ++ show bo ++ ")"
  show (PrTo s) = "switch(" ++ show s ++ ")"
  show (PrInport n) = "inport(" ++ show n ++ ")"
  show (PrUnion pr pr') = "(" ++ show pr ++ ") \\/ (" ++ show pr' ++ ")"
  show (PrIntersect pr pr') = "(" ++ show pr ++ ") /\\ (" ++ show pr' ++ ")"
  show (PrDifference pr pr') = "(" ++ show pr ++ ") // (" ++ show pr' ++ ")"
  show (PrNegate pr) = "~(" ++ show pr ++ ")"

-- 
-- Policies
--

type Actions = Set Port

data Policy p = 
    PoBasic (Predicate p) Actions
  | PoUnion (Policy p) (Policy p)
  | PoIntersect (Policy p) (Policy p)
              
instance Show (Policy p) where
  show (PoBasic e as) = "(" ++ show e ++ ") -> " ++ show as
  show (PoUnion t1 t2) = "(" ++ show t1 ++ ") \\/ (" ++ show t2 ++ ")"
  show (PoIntersect t1 t2) = "(" ++ show t1 ++ ") /\\ (" ++ show t2 ++ ")"

--
-- Interpreter
--
interpretPredicate :: Predicate p -> Transmission p -> Bool

interpretPredicate (PrHeader h w) (Transmission _ _ pkt) = 
  wBitsMake (getHeader pkt h) == w
interpretPredicate (PrInport n) (Transmission _ n' _) = 
  n == n'
interpretPredicate (PrUnion pr1 pr2) t = 
  interpretPredicate pr1 t || interpretPredicate pr2 t
interpretPredicate (PrIntersect pr1 pr2) t = 
  interpretPredicate pr1 t && interpretPredicate pr2 t
interpretPredicate (PrDifference pr1 pr2) t = 
  interpretPredicate pr1 t && not (interpretPredicate pr2 t)
interpretPredicate (PrNegate pr) t = not (interpretPredicate pr t)

interpretPolicy :: Policy p -> Transmission p -> Actions
interpretPolicy (PoBasic pred as) t | interpretPredicate pred t = as
                                    | otherwise = Set.empty
interpretPolicy (PoUnion p1 p2) t = 
  interpretPolicy p1 t `Set.union` interpretPolicy p2 t
interpretPolicy (PoIntersect p1 p2) t = 
  interpretPolicy p1 t `Set.intersection` interpretPolicy p2 t
