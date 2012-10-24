module Frenetic.Slices.Slice
  ( -- * Data Structures
    Slice (..)
  -- * Utilities
  , internalSlice
  , simpleSlice
  , localize
  , switchesOfPredicate
  , poUsesVlans
  , actUsesVlans
  ) where

import Frenetic.Common
import Frenetic.Topo
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Word
import Frenetic.NetCore.Short
import Frenetic.NetCore.Types 
import Frenetic.NetCore.Util
import Frenetic.Pattern

-- |A slice represents a subgraph of the network for the purposes of isolating
-- programs from each other.
--
-- The interface to a slice has two components: a topology comprising switches,
-- ports, and links; and a collection of predicates, one for each outward-facing
-- edge port.
--
-- We represent the topology as a collection of locations in the network, and
-- the predicates as a mapping from locations to predicates.
--
-- Intuitively, packets may travel freely between internal locations, but must
-- satisfy the associated predicate to enter the slice at an ingress location,
-- or leave the slice at an egress location.  If an external port is not listed
-- in the ingress or egress set, then no packets may enter or leave
-- (respectively) on that port.
data Slice = Slice {
  -- |Ports internal to the slice.
  internal :: Set.Set Loc
  -- |External ports, and restrictions on inbound packets.
, ingress :: Map.Map Loc Predicate
  -- |External ports, and restrictions on outbound packets.
, egress  :: Map.Map Loc Predicate } deriving (Eq, Ord, Show)

-- |Produce a slice that exactly covers the given topology, with no ingress or
-- egress ports.
internalSlice :: Graph -> Slice
internalSlice topo = Slice locations Map.empty Map.empty where  
  locations = Set.fromList . map linkToLoc . links $ filterElements isSwitch topo

-- |Produce a slice with all the switches in topo, and predicate applied to all
-- in- and out-bound connections to hosts
simpleSlice :: Graph -> Predicate -> Slice
simpleSlice topo pred = Slice int (Map.fromList $ zip ext (repeat pred))
                                  (Map.fromList $ zip ext (repeat pred))
  where
  -- Only get links coming from switches, and segregate based on pointing to a
  -- host or not.
  (intLinks, extLinks) = partition (\ (_, e, _) -> isSwitch e) $
                         filter (\ (e, _, p) -> isSwitch e) $
                         links topo
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
    mconcat [localizeMods (pred' <&&> Frenetic.NetCore.Types.Switch s) act
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
  Restrict (SendPackets chan) pred -> policy -- TODO(arjun): needs work
  Restrict pol pred -> localize slice (synthRestrict pol pred)
  SendPackets chan -> policy -- TODO(arjun): needs work
  

onSlice :: Slice -> Predicate
onSlice (Slice int ing egr) = prOr .
                              map (\(Loc s p) -> inport s p) .
                              Set.toList .
                              Set.unions $
                              [int, Map.keysSet ing, Map.keysSet egr]

-- |Transform potentially non-local forwarding actions into explicitly local
-- ones on the switch.
localizeMods :: Predicate -> [Action] -> Set.Set Port -> Policy
localizeMods pred actions ports = 
  mconcat $ (pred ==> (forwardPol ++ obs)) : floodPols where
    (allFwds, obs) = partition isForward actions
    (forwards, floods) = partition split allFwds
    -- partial match is safe because we split it
    forwards' = filter (\(Forward (Physical p)  _) -> Set.member p ports)
                 forwards
    forwardPol = forwards'
    floodPols = [mkFlood p mods | p <- Set.toList ports, 
                                  mods <- map getMod floods]
    mkFlood port mods = 
      pred <&&> IngressPort port ==> map f otherPorts where
      f p = Forward (Physical p) mods
      otherPorts = Set.toList $ Set.delete port ports
    split (Forward (Physical _) _) = True
    split _ = False
    getMod (Forward _ m) = m
    getMod _ = error "getMod expected Forward"

-- |Starting with a set of switches, get the switches the predicate might match.
switchesOfPredicate :: Set.Set Switch -> Predicate -> Set.Set Switch
switchesOfPredicate switches pred = case pred of
  Frenetic.NetCore.Types.Switch s -> Set.intersection switches (Set.singleton s)
  Or p1 p2 -> Set.union (switchesOfPredicate switches p1)
                           (switchesOfPredicate switches p2)
  And p1 p2 -> Set.intersection (switchesOfPredicate switches p1)
                                        (switchesOfPredicate switches p2)
  Not _ -> switches
  otherwise -> switches

-- |Determine if a policy ever matches on or sets VLAN tags.
poUsesVlans :: Policy -> Bool
poUsesVlans PoBottom = False
poUsesVlans (PoUnion p1 p2) = poUsesVlans p1 || poUsesVlans p2
poUsesVlans (PoBasic pred action) = prUsesVlans pred || actUsesVlans action
poUsesVlans (Restrict pol pred) = poUsesVlans pol || prUsesVlans pred
poUsesVlans (SendPackets _) = False

-- |Determine if a predicate matches on VLAN tags
prUsesVlans :: Predicate -> Bool
prUsesVlans (DlVlan (Just _)) = True
prUsesVlans (Or p1 p2) = prUsesVlans p1 || prUsesVlans p2
prUsesVlans (And p1 p2) = prUsesVlans p1 || prUsesVlans p2
prUsesVlans (Not p) = prUsesVlans p
prUsesVlans _ = False

actUsesVlans :: [Action] -> Bool
actUsesVlans actions = any f actions
  where f (Forward _ m) = isJust (modifyDlVlan m)
        f _ = False