module Frenetic.Slices.Compile
  ( -- * Compilation
    transform
  , transformEdge
  , dynTransform
  , compileSlice
  , edgeCompileSlice
  -- * Internal tools
  , modifyVlan
  , setVlan
  , matchesSwitch
  ) where

import Control.Monad
import Frenetic.Common
import qualified Data.Map as Map
import qualified Data.Set as Set
import Frenetic.NetCore.Types
import Frenetic.NetCore.Short
import Frenetic.Pattern
import Frenetic.NetCore.Reduce
import Frenetic.NetCore.Pretty
import Frenetic.Slices.Slice
import Frenetic.Slices.VlanAssignment
import Frenetic.Topo

-- |Match a specific vlan tag
vlanMatch :: Vlan -> Predicate
vlanMatch vlan = DlVlan (Just vlan)

-- TODO(astory): also take in the packet channel and handle that
-- |Compile a list of slices and dynamic policies as they change.
dynTransform :: [(Slice, Chan Policy)] -> IO (Chan Policy)
dynTransform combined = do
  updateChan <- newChan :: IO (Chan (Vlan, Policy))
  outputChan <- newChan :: IO (Chan Policy)
  let tagged = zip [1..] combined
  -- Fork off threads to poll the input policy channels, compile them, and write
  -- them into the unified output channel.
  let poll (vlan, (slice, policyChan)) = do
      let loop = do
          update <- readChan policyChan
          let compiled = compileSlice slice vlan update
          writeChan updateChan (vlan, compiled)
      forkIO $ forever $ loop
  mapM_ poll tagged
  -- Poll from the unified channel, and update a map containing the most recent
  -- compiled version of each slice.  After each update, take the union and send
  -- the result down the pipe.
  let loop map = do
      (vlan, compiled) <- readChan updateChan
      let map' = Map.insert vlan compiled map
      writeChan outputChan $ mconcat (Map.elems map')
      loop map'
  forkIO $ loop Map.empty
  return outputChan

-- |Produce the combined policy by compiling a list of slices and policies with
-- the vanilla compiler
transform :: [(Slice, Policy)] -> Policy
transform combined = mconcat policies
  where
    tagged = sequential combined
    policies = map (\(vlan, (slice, policy)) -> compileSlice slice vlan policy)
                   tagged

-- |Produce the combined policy by compiling a list of slices and policies with
-- the edge compiler
transformEdge :: Topo -> [(Slice, Policy)] -> Policy
transformEdge topo combined = mconcat policies where
  tagged = edge topo combined
  policies = map (\(assignment, (slice, policy)) ->
                   edgeCompileSlice slice assignment policy)
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
  reduce $ outport slice safeInportPolicy

-- | Compile a slice with an assignment of VLAN tags to ports.  For this to work
-- properly, the assignment of tags to both ends of an edge must be the same
edgeCompileSlice :: Slice -> Map.Map Loc Vlan -> Policy -> Policy
edgeCompileSlice slice assignment policy = mconcat (queryPols : forwardPols)
  where
    localPolicy = localize slice policy
    -- separate out queries to avoid multiplying them during the fracturing that
    -- goes on in creating the internal and external policies.
    queryPols = queryOnly slice assignment localPolicy
    forwardPols = forwardEdges slice assignment localPolicy

-- |Produce a list of policies that together implement just the query portion of
-- the policy running on the slice
queryOnly :: Slice -> Map.Map Loc Vlan -> Policy -> Policy
queryOnly slice assignment policy = justQueries <%> (onSlice <||> inBound) where
  onSlice = prOr . map onPort . Set.toList $ internal slice
  inBound = ingressPredicate slice <&&> (DlVlan Nothing)
  justQueries = removeForwards policy
  onPort l@(Loc s p) = inport s p <&&> (DlVlan (Just vlan)) <&&>
                       Map.findWithDefault Any l (ingress slice) where
    vlan = case Map.lookup l assignment of
           Just v -> v
           Nothing -> error $
                      "assignment map incomplete at " ++ show l ++
                      "\nmap:   " ++ show assignment ++
                      "\nslice: " ++ show (internal slice)

-- |Remove forwarding actions from policy leaving only queries
removeForwards :: Policy -> Policy
removeForwards PoBottom = PoBottom
removeForwards (PoBasic pred acts) = pred ==> (filter (not.isForward) acts)
removeForwards (PoUnion p1 p2) = PoUnion p1' p2' where
  p1' = removeForwards p1
  p2' = removeForwards p2

-- |Remove queries from policy leaving only forwarding actions
removeQueries :: Policy -> Policy
removeQueries PoBottom = PoBottom
removeQueries (PoBasic pred acts) = pred ==> (filter isForward acts)
removeQueries (PoUnion p1 p2) = PoUnion p1' p2' where
  p1' = removeQueries p1
  p2' = removeQueries p2

-- |Remove forwarding actions to ports other than p
justTo :: Port -> Policy -> Policy
justTo _ PoBottom = PoBottom
justTo p (PoBasic pred acts) = pred ==> (filter pr acts) where
  pr (Forward (Physical p') _) = p == p'
  pr (Forward AllPorts _) = error "AllPorts found while compiling."
  pr _ = True
justTo p (PoUnion p1 p2) = PoUnion p1' p2' where
  p1' = justTo p p1
  p2' = justTo p p2

-- TODO(astory): egress predicates
-- |Produce a list of policies that together instrument the edge-compiled
-- internal policy.  This only considers internal -> internal and internal ->
-- external forwarding.
forwardEdges :: Slice -> Map.Map Loc Vlan -> Policy -> [Policy]
forwardEdges slice assignment policy = concatMap buildPort locs where
  int = internal slice
  ing = Map.keysSet (ingress slice)
  egr = Map.keysSet (egress slice)
  portLookup = portsOfSet (Set.union int (Set.union ing egr))
  locs = Set.toList (Set.union int ing)
  buildPort :: Loc -> [Policy] -- Get the policies for packets to one location
  buildPort l@(Loc s p) = map hop $ Set.toList destinations where
    destinations = case Map.lookup s portLookup of
                     Just dests -> dests
                     Nothing -> error "Port lookup malformed."
    ourVlan = if Set.member l ing then DlVlan Nothing
              else case Map.lookup l assignment of
                     Just v -> DlVlan (Just v)
                     Nothing -> error "Vlan assignment malformed."
    restriction = inport s p <&&>
                  ourVlan <&&>
                  Map.findWithDefault Any l (ingress slice)
    policy' = policy <%> restriction
    hop :: Port -> Policy -- Get the policies for one switch forwarding
    hop port = policy''' where
      loc = Loc s port
      targetVlan = if Set.member loc egr then Nothing
                   else case Map.lookup loc assignment of
                          Just v -> Just v
                          Nothing -> error "Vlan assignment malformed."
      policy'' = justTo port policy'
      -- It's safe to use modifyVlan because we only have actions on this one
      -- forwarding hop.
      policy''' = modifyVlan targetVlan policy''

portsOfSet :: Set.Set Loc -> Map.Map Switch (Set.Set Port)
portsOfSet = Map.fromListWith Set.union .
             map (\(Loc s p) -> (s, Set.singleton p)) .
             Set.toList

-- |Produce a policy that only considers traffic on this vlan and on internal
-- ports.  Note that if the policy does not modify vlans, then it also only
-- emits traffic on this vlan.
isolate :: Slice -> Vlan -> Policy -> Policy
isolate slice vlan policy = policy <%> (vlPred <&&> intern)
  where
    vlPred = vlanMatch vlan
    intern = prOr . map (\(Loc s p) -> inport s p) . Set.toList $
             internal slice

locToPred :: Loc -> Predicate
locToPred (Loc switch port) = inport switch port

-- |Produce a policy that moves packets into the vlan as defined by the slice's
-- input policy.
inportPo :: Slice -> Vlan -> Policy -> Policy
inportPo slice vlan policy =
  let incoming = ingressPredicate slice in
  let policyIntoVlan = modifyVlan (Just vlan) policy in
  policyIntoVlan <%> (incoming <&&> DlVlan Nothing)

-- |Produce a new policy the same as the old, but wherever a packet leaves an
-- outgoing edge, unset its VLAN.  Precondition:  every PoBasic must match at
-- most one switch.
outport :: Slice -> Policy -> Policy
outport slice policy = foldr stripVlan policy locs
  where locs = Map.keys (egress slice)

-- |Produce a predicate matching any of the inports (and their predicate)
-- specified
ingressPredicate :: Slice -> Predicate
ingressPredicate slice =
  prOr . map ingressSpecToPred . Map.assocs $ ingress slice

-- |Produce a predicate matching the ingress predicate at a particular location
ingressSpecToPred :: (Loc, Predicate) -> Predicate
ingressSpecToPred (loc, pred) = And pred (locToPred loc)

-- |Walk through the policy and globally set VLAN to vlan at each forwarding
-- action
modifyVlan :: Maybe Vlan -> Policy -> Policy
modifyVlan _ PoBottom = PoBottom
modifyVlan vlan (PoBasic pred acts) = pred ==> (map f acts)
  where f (Forward p m) = Forward p (m { modifyDlVlan = Just vlan })
        f act = act
modifyVlan vlan (PoUnion p1 p2) = PoUnion (modifyVlan vlan p1)
                                          (modifyVlan vlan p2)

-- |Unset vlan for packets forwarded to location (without link transfer) and
-- leave rest of policy unchanged.  Note that this assumes that each PoBasic
-- matches at most one switch.
stripVlan :: Loc -> Policy -> Policy
stripVlan = setVlan Nothing

-- |Set vlan tag for packets forwarded to location (without link transfer) and
-- leave rest of policy unchanged.  Note that this assumes that each PoBasic
-- matches at most one switch.
setVlan :: Maybe Vlan -> Loc -> Policy -> Policy
setVlan _ _ PoBottom = PoBottom
setVlan vlan loc (PoUnion p1 p2) = PoUnion (setVlan vlan loc p1)
                                           (setVlan vlan loc p2)
setVlan vlan (Loc switch port) pol@(PoBasic pred acts) =
  if matchesSwitch switch pred then PoBasic pred m'
                               else pol
  where
    m' = map setVlanOnPort acts
    setVlanOnPort (Forward (Physical p) mod) =
      if p == port then (Forward (Physical p) mod {modifyDlVlan = Just vlan})
                   else (Forward (Physical p) mod)
    setVlanOnPort (Forward AllPorts mod) =
      error "AllPorts encountered in slice compilation.  Did you localize?"
    setVlanOnPort act = act

-- |Determine if a predicate can match any packets on a switch (overapproximate)
matchesSwitch :: Switch -> Predicate -> Bool
matchesSwitch s1 (Switch s2)          = s1 == s2
matchesSwitch s (Or p1 p2)     = matchesSwitch s p1 || matchesSwitch s p2
matchesSwitch s (And p1 p2) = matchesSwitch s p1 && matchesSwitch s p2
matchesSwitch s (Not _)        = True
matchesSwitch s _ = True