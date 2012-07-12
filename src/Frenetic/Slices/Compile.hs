module Frenetic.Slices.Compile
  (compileSlice
  ) where

import Data.Word
import Frenetic.NetCore
import Frenetic.Slices.Slice
import qualified Data.MultiSet as MS
import qualified Data.Set as Set
import qualified Data.Map as Map

type Vlan = Word16

-- |Match a specific vlan tag
vlanMatch :: Vlan -> Predicate
vlanMatch vlan = PrPattern (top {ptrnDlVlan = exact vlan})

transform :: [(Slice, Policy)] -> Policy
transform combined = 
  poNaryUnion policies
  where
    -- TODO(astory): throw error on running out of VLANs
    tagged = zip [1..4095] combined
    policies = map (\(vlan, (slice, policy)) -> compileSlice slice vlan policy)
                   tagged

-- TODO(astory): validate policies against their slice, or maybe just restrict
-- them.
-- |Compile a slice with a vlan key
compileSlice :: Slice -> Vlan -> Policy -> Policy
compileSlice slice vlan policy =
  let safePolicy = isolate vlan policy in
  let inportPolicy = inport slice vlan policy in
  let safeInportPolicy = PoUnion safePolicy inportPolicy in
  outport slice safeInportPolicy

-- |Produce a policy that only considers traffic on this vlan
-- Note that if the policy does not modify vlans, then it also only emits
-- traffic on this vlan.
isolate :: Vlan -> Policy -> Policy
isolate vlan policy = policy `poRestrict` (vlanMatch vlan)

-- |Produce a policy that moves packets into the vlan as defined by the slice's
-- input policy.
inport :: Slice -> Vlan -> Policy -> Policy
inport slice vlan policy =
  let incoming = ingressPredicate slice in
  let policyIntoVlan = modifyVlan vlan policy in
  policyIntoVlan `poRestrict` incoming

-- |Produce a new policy the same as the old, but wherever a packet leaves an
-- outgoing edge, set its VLAN to 0.
outport :: Slice -> Policy -> Policy
outport slice policy = foldr stripVlan policy locs
  where locs = Map.keys (egress slice)

-- |Produce a predicate matching any of the inports (and their predicate)
-- specified
ingressPredicate :: Slice -> Predicate
ingressPredicate slice = 
  prNaryUnion . map ingressSpecToPred . Map.assocs $ ingress slice

-- |Produce a predicate matching the ingress predicate at a particular location
ingressSpecToPred :: (Loc, Predicate) -> Predicate
ingressSpecToPred (Loc switch port, pred) = PrIntersect swPred pred'
  where
    swPred = PrTo switch
    pred' = PrIntersect (PrPattern (top {ptrnInPort = Just port})) pred

-- |Walk through the policy and globally set VLAN to vlan at each forwarding
-- action
modifyVlan :: Vlan -> Policy -> Policy
modifyVlan _ PoBottom = PoBottom
modifyVlan vlan (PoBasic pred (Action (ForwardPorts m) obs)) =
  PoBasic pred (Action (ForwardPorts m') obs)
    where
      m' = MS.map setVlans m
      setVlans (p, mod) = (p, setVlan mod)
      setVlan pattern = pattern {ptrnDlVlan = exact vlan}
modifyVlan vlan (PoUnion p1 p2) = PoUnion (modifyVlan vlan p1)
                                          (modifyVlan vlan p2)

-- |Set vlan to 0 for packets forwarded to location (without link transfer)
stripVlan :: Loc -> Policy -> Policy
stripVlan = setVlan 0

-- |Set vlan tag for packets forwarded to location (without link transfer)
setVlan :: Vlan -> Loc -> Policy -> Policy
setVlan _ _ PoBottom = PoBottom
setVlan vlan (Loc switch port) (PoBasic pred (Action (ForwardPorts m) obs)) =
  PoBasic pred (Action (ForwardPorts m') obs)
  where
    m' = MS.map setVlanOnPort m
    setVlanOnPort (Physical p, mod) = (Physical p, mod{ptrnDlVlan = exact vlan})
    setVlanOnPort (PhysicalFlood, mod) =
      error "Cannot compile slices with FLOOD."
setVlan value loc (PoUnion p1 p2) = PoUnion (setVlan value loc p1)
                                            (setVlan value loc p2)
