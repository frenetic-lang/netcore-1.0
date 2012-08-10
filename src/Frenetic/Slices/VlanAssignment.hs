module Frenetic.Slices.VlanAssignment
  ( sequential
  , edge
  ) where

import Data.Maybe
import Data.Word
import qualified Data.Map as Map
import qualified Data.Set as Set
import Frenetic.NetCore.Short
import Frenetic.NetCore.Types
import Frenetic.Slices.Slice
import Frenetic.Topo

maxVlan :: Vlan
maxVlan = 2^12

sequential :: [(Slice, Policy)] -> [(Vlan, (Slice, Policy))]
sequential combined =
  if length combined > fromIntegral maxVlan
    then error ((show $ length combined) ++
                " is too many VLANs to compile sequentially.")
    else zip [1..maxVlan] combined

type Edge = (Loc, Loc)

edge :: Topo -> [(Slice, Policy)] -> [(Map.Map Loc Vlan, (Slice, Policy))]
edge topo combined = paired  where
  locUse :: Map.Map Loc (Set.Set (Slice, Policy))
  locUse =  foldr addEdges Map.empty combined

  edgeUse :: Map.Map Edge (Set.Set (Slice, Policy))
  -- getEdge returns the normal form (smallest first)
  edgeUse = Map.mapKeysWith Set.union (getEdge topo) locUse

  vlanEdges :: Map.Map Edge (Map.Map (Slice, Policy) Vlan)
  vlanEdges = Map.map assign edgeUse

  vlans :: Map.Map Loc (Map.Map (Slice, Policy) Vlan)
  vlans = Map.fromList .
          concat .
          map (\ ((l1, l2), v) -> [(l1, v), (l2, v)]) .
          Map.toList $
          vlanEdges

  bySlice :: Map.Map (Slice, Policy) (Map.Map Loc Vlan)
  bySlice = invert vlans

  paired = [ (lookup, both) | (both, lookup) <- Map.toList bySlice]

-- | Add (loc, slice) to map for all internal locations in slice
addEdges :: (Slice, Policy) -> Map.Map Loc (Set.Set (Slice, Policy)) ->
                               Map.Map Loc (Set.Set (Slice, Policy))
addEdges (slice, policy) m = Map.unionWith Set.union (Map.fromList locations) m where
  locations = map (\l -> (l, Set.singleton (slice, policy))) .
              Set.toList $ internal slice

assign :: Set.Set (Slice, Policy) -> Map.Map (Slice, Policy) Vlan
assign slices =
  if Set.size slices > fromIntegral maxVlan
    then error ((show $ Set.size slices) ++
                " is too many VLANs to compile sequentially.")
    else Map.fromList $ zip (Set.toList slices) [1..maxVlan]

invert :: (Ord a, Ord b) => Map.Map a (Map.Map b v) -> Map.Map b (Map.Map a v)
invert m = m' where
  associations = map (\(k, submap) -> (k, Map.toList submap)) . Map.toList $ m
  m' = Map.fromListWith Map.union
       [(b, Map.singleton a v) | (a, bs) <- associations, (b, v) <- bs]
