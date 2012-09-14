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
import Frenetic.Common

maxVlan :: Vlan
maxVlan = 2^12

sequential :: [(Slice, Policy)] -> [(Vlan, (Slice, Policy))]
sequential combined =
  if length combined > fromIntegral maxVlan
    then error (show (length combined) ++
                " is too many VLANs to compile sequentially.")
    else zip [1..maxVlan] combined

type Edge = (Loc, Loc)

edge :: Topo -> [(Slice, Policy)] -> [(Map.Map Loc Vlan, (Slice, Policy))]
edge topo slices = paired  where
  indexedSlices :: [(Int, Slice, Policy)]
  indexedSlices = zipWith (\n (s,p) -> (n,s,p)) [0 .. ] slices
  
  -- Slice and policy at each (switch, port)
  locUse :: Map Loc (Map Int (Slice, Policy))
  locUse = foldr addEdges Map.empty indexedSlices

  -- Slice and policy at each edge (switch,port) --- (switch,port)
  edgeUse :: Map Edge (Map Int (Slice, Policy))
  -- getEdge returns the normal form (smallest first)
  edgeUse = Map.mapKeysWith Map.union (getEdge topo) locUse
  -- It is safe to use Map.union above. Any conflict between keys will be
  -- spurious, since a key always indexes the same slice.

  vlanEdges :: [(Edge, Map Int (Slice, Policy, Vlan))]
  vlanEdges = map (\(e,m) -> (e, assign m)) (Map.toList edgeUse)

  vlans :: Map Loc (Map Int (Slice, Policy, Vlan))
  vlans = Map.fromList $
            concatMap (\((loc1, loc2), v) -> [(loc1, v), (loc2, v)])
                      vlanEdges

  locations = Map.keys vlans

  bySlice :: [(Int, Slice, Policy, Map Loc Vlan)]
  bySlice = map f indexedSlices where
              f (ix, s, p) = (ix, s, p, foldl g Map.empty locations) where
                g vlanMap loc = case Map.lookup loc vlans of
                  Nothing -> error "bySlice expected to find location"
                  Just map -> case Map.lookup ix map of
                    Nothing -> vlanMap
                    Just (_, _, vlan) -> Map.insert loc vlan vlanMap

  paired = map (\(_, s, p, vlanMap) -> (vlanMap, (s, p))) bySlice       

-- | Add (loc, slice) to map for all internal locations in slice
addEdges :: (Int, Slice, Policy)
         -> Map Loc (Map Int (Slice, Policy))
         -> Map Loc (Map Int (Slice, Policy))
addEdges (ix, slice, policy) byLoc = 
  Map.unionWith Map.union byLoc thisSliceByLoc
    where thisSlice = Map.singleton ix (slice, policy)
          thisSliceByLoc = Map.fromList $
            map (\loc -> (loc, thisSlice))
                (Set.toList (internal slice))

assign :: Map Int (Slice, Policy)
       -> Map Int (Slice, Policy, Vlan)
assign slices =
  if Map.size slices > fromIntegral maxVlan then
    error (show (Map.size slices) ++ 
           " is too many VLANs to compile sequentially.")
  else 
    snd $ Map.mapAccum (\vlan (s, pol) -> (succ vlan, (s, pol, vlan))) 1 slices

invert :: (Ord a, Ord b) => Map.Map a (Map.Map b v) -> Map.Map b (Map.Map a v)
invert m = m' where
  associations = map (\(k, submap) -> (k, Map.toList submap)) . Map.toList $ m
  m' = Map.fromListWith Map.union
       [(b, Map.singleton a v) | (a, bs) <- associations, (b, v) <- bs]
