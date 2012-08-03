module Frenetic.Slices.Slice
  ( -- * Data Structures
    Slice (..)
  , Loc (..)
  -- * Utilities
  , internalSlice
  , simpleSlice
  , localize
  , switchesOfPredicate
  , poUsesVlans
  , actUsesVlans
  ) where

import Data.Graph.Inductive.Graph
import Data.List
import qualified Data.Map as Map
import qualified Data.MultiSet as MS
import qualified Data.Set as Set
import Data.Word
import Frenetic.NetCore
import Frenetic.Pattern
import Frenetic.Topo

data Slice = Slice {
  -- |Ports internal to the slice.
  internal :: Set.Set Loc
  -- |External ports, and restrictions on inbound packets.
, ingress :: Map.Map Loc Predicate
  -- |External ports, and restrictions on outbound packets.
, egress  :: Map.Map Loc Predicate
} deriving (Eq, Ord, Show)

linkToLoc (n, _, p) = Loc (fromIntegral n) p

-- |Produce a slice that exactly covers the given topology, with no ingress or
-- egress ports.
internalSlice :: Topo -> Slice
internalSlice topo = Slice locations Map.empty Map.empty where
  locations = Set.fromList . map linkToLoc . labEdges $ topo

-- |Produce a slice with all the switches in topo, and predicate applied to all
-- in- and out-bound connections to hosts
simpleSlice :: Topo -> Predicate -> Slice
simpleSlice topo pred = Slice int (Map.fromList $ zip ext (repeat pred))
                                  (Map.fromList $ zip ext (repeat pred))
  where
  links = labEdges topo
  -- Only get links coming from switches, and segregate based on pointing to a
  -- host or not.
  (intLinks, extLinks) = partition (\(_, n2, _) -> not $ isHost topo n2) .
                         filter (\(_, _, p) -> p /= 0) $
                         links
  int = Set.fromList $ map linkToLoc intLinks
  ext = map linkToLoc extLinks

-- |Transform policy running on slice into a FLOOD- and unbound port-free (i.e.,
-- every port has an unambiguous switch associated with it) version with the
-- same semantics, assuming it runs on the subgraph described by the slice.  In
-- the returned policy, all forwarding actions within the slice, or to one of
-- its egress ports.
localize :: Slice -> Policy -> Policy
localize slice policy = case policy of
  PoBottom -> PoBottom
  PoUnion p1 p2 -> localize slice p1 <+> localize slice p2
  PoBasic pred act ->
    let ss = Set.toList (switchesOfPredicate switches pred) in
    unions [localizeMods (pred' <&&> PrTo s) act
                              (Map.findWithDefault Set.empty s ports)
                | s <- ss]
    where
      switches = Set.map (\ (Loc s _) -> s) locations
      ports =
        Set.foldr (\ (Loc s p) -> Map.insertWith Set.union s (Set.singleton p))
                  Map.empty locations
      locations = Set.union (internal slice)
                            (Set.fromList . Map.keys $ egress slice)
      pred' = pred <&&> onSlice slice

onSlice :: Slice -> Predicate
onSlice (Slice int ing egr) = prOr .
                              map (\(Loc s p) -> inport s p) .
                              Set.toList .
                              Set.unions $
                              [int, Map.keysSet ing, Map.keysSet egr]

-- |Transform potentially non-local forwarding actions into explicitly local
-- ones on the switch.
localizeMods :: Predicate -> Action -> Set.Set Port -> Policy
localizeMods pred (Action m obs) ports = unions (forwardPol : floodPols)
  where
  (forwards, floods) = MS.mapEither split m
  -- partial match is safe because we split it
  forwards' = MS.filter (\(Physical p, _) -> Set.member p ports) forwards
  -- Put the observations with the forwards so we don't duplicate them
  forwardPol = pred ==> Action forwards' obs
  floodPols = [mkFlood p mods | p <- Set.toList ports, mods <- MS.toList floods]
  mkFlood port mods = pred <&&> inPort port ==> Action mods' MS.empty where
    mods' = MS.fromList [(Physical p, mods) | p <- otherPorts]
    otherPorts = Set.toList $ Set.delete port ports

split (Physical p, rewrite) = Left (Physical p, rewrite)
split (AllPorts, rewrite) = Right (rewrite)

-- |Starting with a set of switches, get the switches the predicate might match.
switchesOfPredicate :: Set.Set Switch -> Predicate -> Set.Set Switch
switchesOfPredicate switches pred = case pred of
  PrPattern _       -> switches
  PrTo s            -> Set.intersection switches (Set.singleton s)
  PrUnion p1 p2     -> Set.union (switchesOfPredicate switches p1)
                           (switchesOfPredicate switches p2)
  PrIntersect p1 p2 -> Set.intersection (switchesOfPredicate switches p1)
                                        (switchesOfPredicate switches p2)
  PrNegate _        -> switches

-- |Determine if a policy ever matches on or sets VLAN tags.
poUsesVlans :: Policy -> Bool
poUsesVlans PoBottom = False
poUsesVlans (PoUnion p1 p2) = poUsesVlans p1 || poUsesVlans p2
poUsesVlans (PoBasic pred action) = prUsesVlans pred || actUsesVlans action

-- |Determine if a predicate matches on VLAN tags
prUsesVlans :: Predicate -> Bool
prUsesVlans (PrPattern pat) = pat {ptrnDlVlan = wild} /= pat
prUsesVlans (PrTo _) = False
prUsesVlans (PrUnion p1 p2) = prUsesVlans p1 || prUsesVlans p2
prUsesVlans (PrIntersect p1 p2) = prUsesVlans p1 || prUsesVlans p2
prUsesVlans (PrNegate p) = prUsesVlans p

actUsesVlans :: Action -> Bool
actUsesVlans (Action ms _) =
  any (\(_, m) -> modifyDlVlan m /= Nothing) $ MS.toList ms
