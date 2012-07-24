module Frenetic.Slices.Compile
  ( -- * Compilation
    transform
  , compileSlice
  -- * Internal tools
  , modifyVlan
  , setVlan
  , matchesSwitch
  ) where

import qualified Data.Map as Map
import qualified Data.MultiSet as MS
import qualified Data.Set as Set
import Data.Word
import Frenetic.NetCore
import Frenetic.NetCore.Reduce
import Frenetic.NetCore.Short
import Frenetic.Slices.Slice

maxVlan :: Vlan
maxVlan = maxBound

-- |Match a specific vlan tag
vlanMatch :: Vlan -> Predicate
vlanMatch vlan = dlVlan vlan

transform :: [(Slice, Policy)] -> Policy
transform combined =
  if length combined > fromIntegral maxVlan
    then error "Too many VLANs to compile"
    else poNaryUnion policies
  where
    tagged = zip [1..maxVlan] combined
    policies = map (\(vlan, (slice, policy)) -> compileSlice slice vlan policy)
                   tagged

-- TODO(astory): egress predicates
-- |Compile a slice with a vlan key
compileSlice :: Slice -> Vlan -> Policy -> Policy
compileSlice slice vlan policy =
  if poUsesVlans policy then error "input policy uses VLANs." else
  let localPolicy = localize slice policy in
  -- A postcondition of localize is that all the forwarding actions of the
  -- policy make sense wrt the slice, and that every PoBasic matches at most one
  -- switch.  This is a precondition for outport
  let safePolicy = isolate slice vlan localPolicy in
  let inportPolicy = inportPo slice vlan localPolicy in
  let safeInportPolicy = PoUnion safePolicy inportPolicy in
  outport slice safeInportPolicy

-- |Produce a policy that only considers traffic on this vlan and on internal
-- ports.  Note that if the policy does not modify vlans, then it also only
-- emits traffic on this vlan.
isolate :: Slice -> Vlan -> Policy -> Policy
isolate slice vlan policy = policy % (vlPred <&> intern)
  where
    vlPred = vlanMatch vlan
    intern = prNaryUnion . map (\(Loc s p) -> inport s p) . Set.toList $
             internal slice

locToPred :: Loc -> Predicate
locToPred (Loc switch port) = inport switch port

-- |Produce a policy that moves packets into the vlan as defined by the slice's
-- input policy.
inportPo :: Slice -> Vlan -> Policy -> Policy
inportPo slice vlan policy =
  let incoming = ingressPredicate slice in
  let policyIntoVlan = modifyVlan vlan policy in
  policyIntoVlan % (incoming <&> dlVlan 0)

-- |Produce a new policy the same as the old, but wherever a packet leaves an
-- outgoing edge, set its VLAN to 0.  Precondition:  every PoBasic must match at
-- most one switch.
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
ingressSpecToPred (loc, pred) = PrIntersect pred (locToPred loc)

-- |Walk through the policy and globally set VLAN to vlan at each forwarding
-- action
modifyVlan :: Vlan -> Policy -> Policy
modifyVlan _ PoBottom = PoBottom
modifyVlan vlan (PoBasic pred (Action m obs)) =
  PoBasic pred (Action m' obs)
    where
      m' = MS.map setVlans m
      setVlans (p, mod) = (p, setVlan mod)
      setVlan pattern = pattern {ptrnDlVlan = Exact vlan}
modifyVlan vlan (PoUnion p1 p2) = PoUnion (modifyVlan vlan p1)
                                          (modifyVlan vlan p2)

-- |Set vlan to 0 for packets forwarded to location (without link transfer) and
-- leave rest of policy unchanged.  Note that this assumes that each PoBasic
-- matches at most one switch.
stripVlan :: Loc -> Policy -> Policy
stripVlan = setVlan 0

-- |Set vlan tag for packets forwarded to location (without link transfer) and
-- leave rest of policy unchanged.  Note that this assumes that each PoBasic
-- matches at most one switch.
setVlan :: Vlan -> Loc -> Policy -> Policy
setVlan _ _ PoBottom = PoBottom
setVlan vlan loc (PoUnion p1 p2) = PoUnion (setVlan vlan loc p1)
                                           (setVlan vlan loc p2)
setVlan vlan (Loc switch port) pol@(PoBasic pred (Action m obs)) =
  if matchesSwitch switch pred then PoBasic pred (Action m' obs)
                               else pol
  where
    m' = MS.map setVlanOnPort m
    setVlanOnPort (Physical p, mod) =
      if p == port then (Physical p, mod {ptrnDlVlan = Exact vlan})
                   else (Physical p, mod)
    setVlanOnPort (PhysicalFlood, mod) =
      error "FLOOD encountered in slice compilation.  Did you first localize?"

-- |Determine if a predicate can match any packets on a switch (overapproximate)
matchesSwitch :: Switch -> Predicate -> Bool
matchesSwitch _ (PrPattern _)       = True
matchesSwitch s1 (PrTo s2)          = s1 == s2
matchesSwitch s (PrUnion p1 p2)     = matchesSwitch s p1 || matchesSwitch s p2
matchesSwitch s (PrIntersect p1 p2) = matchesSwitch s p1 && matchesSwitch s p2
matchesSwitch s (PrNegate _)        = True
