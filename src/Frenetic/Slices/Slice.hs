module Frenetic.Slices.Slice
  ( -- * Data Structures
    Slice (..)
  , Loc (..)
  -- * Utilities
  , localize
  , switchesOfPredicate
  , poUsesVlans
  ) where

import Data.Word
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.MultiSet as MS
import Frenetic.NetCore.API
import Frenetic.Pattern

data Slice = Slice {
  -- |Ports internal to the slice.
  internal :: Set.Set Loc
  -- |External ports, and restrictions on inbound packets.
, ingress :: Map.Map Loc Predicate
  -- |External ports, and restrictions on outbound packets.
, egress  :: Map.Map Loc Predicate
}

-- |Transform policy running on slice into a FLOOD- and unbound port-free (i.e.,
-- every port has an unambiguous switch associated with it) version with the
-- same semantics, assuming it runs on the subgraph described by the slice.  In
-- the returned policy, all forwarding actions within the slice, or to one of
-- its egress ports.
localize :: Slice -> Policy -> Policy
localize slice policy = case policy of
  PoBottom -> PoBottom
  PoUnion p1 p2 -> localize slice p1 <+> localize slice p2
  PoBasic pred (Action m obs) ->
    let ss = Set.toList (switchesOfPredicate switches pred) in
    poNaryUnion [pred <&> (PrTo s) ==> Action (localizeMods m ports s) obs
                 |s <- ss]
  where
    switches = Set.map (\ (Loc s _) -> s) locations
    ports =
      Set.foldr (\ (Loc s p) -> Map.insertWith Set.union s (Set.singleton p))
                Map.empty locations
    locations = Set.union (internal slice)
                          (Set.fromList . Map.keys $ egress slice)

-- |Transform potentially non-local forwarding actions into explicitly local
-- ones on the switch.
localizeMods :: Forward -> Map.Map Switch (Set.Set Port) -> Switch -> Forward
localizeMods m ports switch = MS.unionsMap localizeMod m where
  localizeMod (port, mods) =
    let ps = Map.findWithDefault Set.empty switch ports in
    case port of
      Physical p -> if Set.member p ps then MS.singleton (Physical p, mods)
                                       else MS.empty
      PhysicalFlood -> MS.fromList [(Physical p, mods) | p <- Set.toList ps]

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
  MS.fold (\ (_, m) accum -> accum || m {ptrnDlVlan = wild} /= m) False ms
