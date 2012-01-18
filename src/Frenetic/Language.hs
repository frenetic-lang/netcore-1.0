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

import System.IO.Unsafe
import System.IO 
import Data.Bits
import Data.Set as Set

import Frenetic.Network

--
-- Predicates
--
data Predicate p =
    forall b. (Bits b) => PrHeader (Header b) (Maybe b)
  | PrTo Switch
  | EInport Port
  | PrUnion (Predicate p) (Predicate p)
  | PrIntersect (Predicate p) (Predicate p)
  | PrDifference (Predicate p) (Predicate p)
  | PrNegate (Predicate p)

instance Show (Predicate pack) where
  show (PrHeader h bo) = "(" ++ show h ++ " : " ++ show bo ++ ")"
  show (PrTo s) = "switch(" ++ show s ++ ")"
  show (EInport n) = "inport(" ++ show n ++ ")"
  show (PrUnion pr pr') = "(" ++ show pr ++ ") \\/ (" ++ show pr' ++ ")"
  show (PrIntersect pr pr') = "(" ++ show pr ++ ") /\\ (" ++ show pr' ++ ")"
  show (PrDifference pr pr') = "(" ++ show pr ++ ") // (" ++ show pr' ++ ")"
  show (PrNegate pr) = "~(" ++ show pr ++ ")"

--
-- Algebras
-- 
class Lattice a where 
  top :: a
  bottom :: a
  meet :: a -> a -> a
  join :: a -> a -> a 

(\/) :: (Lattice a) => a -> a -> a
(\/) = join

(/\) :: (Lattice a) => a -> a -> a
(/\) = meet

class (Lattice a) => BooleanAlgebra a where
  neg :: a -> a
  minus :: a -> a -> a
  neg a = top `minus` a
  minus a b = a `meet` (neg b)

(//) :: (BooleanAlgebra a) => a -> a -> a
(//) = minus

instance Lattice Bool where
  top = True
  bottom = False
  meet = (&&)
  join = (||)

instance BooleanAlgebra Bool where
  neg = not
  
instance (Lattice a, Eq a) => Lattice (Maybe a) where
  top = Just top
  bottom = Just bottom
  join x Nothing | x == top = top
                 | otherwise = Nothing
  join Nothing x | x == top = top
                 | otherwise = Nothing
  join (Just x) (Just y) = Just $ join x y
  meet x Nothing | x == bottom = bottom
                 | otherwise = Nothing
  meet Nothing x | x == bottom = bottom
                 | otherwise = Nothing
  meet (Just x) (Just y) = Just $ meet x y

--
-- CoSets
--
data CoSet a = PSet (Set a) | NSet (Set a)

instance Show a => Show (CoSet a) where
  show (PSet s) = show (toList s)
  show (NSet s) | Set.null s = "Universe"
                | otherwise = "~" ++ show (toList s)

instance Ord a => Lattice (CoSet a) where
  top = NSet Set.empty
  bottom = PSet Set.empty
  join (PSet s1) (PSet s2) = PSet (Set.union s1 s2) 
  join (NSet s1) (NSet s2) = NSet (Set.intersection s1 s2) 
  join (PSet s1) (NSet s2) = NSet (Set.difference s2 s1) 
  join (NSet s1) (PSet s2) = NSet (Set.difference s1 s2) 
  meet (PSet s1) (PSet s2) = PSet (Set.intersection s1 s2) 
  meet (NSet s1) (NSet s2) = NSet (Set.union s1 s2) 
  meet (PSet s1) (NSet s2) = PSet (Set.difference s1 s2) 
  meet (NSet s1) (PSet s2) = PSet (Set.difference s2 s1) 

cosetToList :: CoSet a -> [a]
cosetToList (PSet s) = Set.toList s
coSetToList cs = error ("No support for cofinite sets" ++ show cs)

--
-- Actions
--
data Action = 
    AForward Port
  deriving (Eq, Ord)

instance Show Action where
  show (AForward p) = "forward(" ++ show p ++ ")"

type Actions = CoSet Action

-- 
-- Policies
--
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
interpretPredicate (PrHeader _ Nothing) _ = 
  True
interpretPredicate (PrHeader h (Just b)) (Transmission _ _ pkt) = 
  getHeader pkt h == b 
interpretPredicate (EInport n) (Transmission _ n' _) = 
  n == n'
interpretPredicate (PrUnion p1 p2) t = 
  interpretPredicate p1 t \/ interpretPredicate p2 t
interpretPredicate (PrIntersect p1 p2) t = 
  interpretPredicate p1 t /\ interpretPredicate p2 t
interpretPredicate (PrDifference p1 p2) t = 
  interpretPredicate p1 t // (interpretPredicate p2 t)
interpretPredicate (PrNegate p1) t = 
  interpretPredicate p1 t 

interpretPolicy :: Policy p -> Transmission p -> Actions
interpretPolicy (PoBasic pred as) t = 
  if interpretPredicate pred t then as else PSet (Set.empty)
interpretPolicy (PoUnion p1 p2) t = 
  interpretPolicy p1 t \/ interpretPolicy p2 t
interpretPolicy (PoIntersect p1 p2) t = 
  interpretPolicy p1 t /\ interpretPolicy p2 t
