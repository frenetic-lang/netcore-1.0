module Tests.Frenetic.Slices.TestVlanAssignment where

import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Test.HUnit hiding (Node)
import Test.Framework.Providers.HUnit

import qualified Data.Map as Map
import qualified Data.Set as Set

import Frenetic.NetCore
import Frenetic.Slices.Slice
import Frenetic.Slices.VlanAssignment
import Frenetic.TopoGen

vlanAssignmentTests = $(testGroupGenerator)

case_testBasicSequential = do
  let slices = [internalSlice (linear i) | i <- [1 .. 10]]
  let policies = repeat PoBottom
  let combined = zip slices policies
  let result = sequential combined
  assertEqual "inputs unchanged" combined (map snd result)
  assertEqual "no shared vlans" (length slices)
                                (Set.size . Set.fromList . map fst $ result)

findShared s (rest, Nothing) = if Set.size (Set.intersection s rest) > 0
                                 then (rest, Just (Set.intersection s rest))
                                 else (Set.union s rest, Nothing)
findShared _ (r, Just s) = (r, Just s)

case_testBasicEdge = do
  let topo = kComplete 10
  let slices = take 10 . repeat $ internalSlice topo
  -- Policies don't matter, just need to be non-identical
  let policies = [top ==> forward [i] | i <- [1..10]]
  let combined = zip slices policies
  let result = edge topo combined
  assertEqual "inputs unchanged" combined (map snd result)
  let sliceLocVlans = [Set.fromList . Map.toList $ m | (m, _) <- result]
  let (_, mShared) = foldr findShared (Set.empty, Nothing) sliceLocVlans
  assertEqual "No shared locations and vlans" Nothing mShared
