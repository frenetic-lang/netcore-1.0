-------------------------------
-- | Tools for verifying properties of slices without using SMT
module Frenetic.Slices.Verification
  ( separated
  , edgeDisjoint
  ) where

import Frenetic.NetCore
import Frenetic.Slices.Slice
import Data.Set as Set
import Data.Map as Map

-- |Determine if two slices use a disjoint set of switches
separated :: Slice -> Slice -> Bool
separated slice1 slice2 = Set.intersection s1 s2 == Set.empty where
  s1 = switchesOfSlice slice1
  s2 = switchesOfSlice slice2

-- |Determine if two slices use disjoint edges (and therefore do not require
-- traffic disambiguation)
edgeDisjoint :: Slice -> Slice -> Bool
edgeDisjoint slice1 slice2 = Set.intersection s1 s2 == Set.empty where
  s1 = Set.map (\ (Loc s _) -> s) (internalPorts slice1)
  s2 = Set.map (\ (Loc s _) -> s) (internalPorts slice2)

-- |Get all switches of a slice
switchesOfSlice :: Slice -> Set.Set Switch
switchesOfSlice (Slice internal ingress egress) =
  Set.map (\ (Loc s _) -> s)
    (Set.union (internal)
    (Set.union (Map.keysSet ingress)
               (Map.keysSet egress)))
