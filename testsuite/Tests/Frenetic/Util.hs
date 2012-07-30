module Tests.Frenetic.Util
  (-- * Constructors without queries
    linear
  , linearHosts
  , kComplete
  -- * Constructors with queries
  , linearQ
  , linearHostsQ
  , kCompleteQ
  , kCompleteQHosts
  ) where

import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map as Map
import Frenetic.NetCore.API
import Frenetic.NetCore.Short
import Frenetic.Pattern
import Frenetic.PolicyGen
import Frenetic.Slices.Slice
import Frenetic.Topo
import qualified Frenetic.TopoGen as TG

import Data.Graph.Inductive.Graph

linear :: Integral n => [[n]] -> (Topo, [Policy])
linear nodess = (topo, policies) where
  nodess' = map Set.fromList nodess
  max = maximum . concat . map Set.toList $ nodess'
  topo = TG.linear (max + 1)
  policies = map mkPolicy nodess'
  mkPolicy nodes = simuFlood $ subgraph (Set.map fromIntegral nodes) topo

linearHosts :: Integral n => [[n]] -> (Topo, [(Slice, Policy)])
linearHosts nodess = (topo,
                      map (\ns -> (mkSlice ns, mkPolicy ns)) nodess')
  where
  nodess' = map Set.fromList nodess
  max = maximum . concat . map Set.toList $ nodess'
  topo = TG.linearHosts (max + 1)
  mkPolicy nodes = simuFlood $ subgraph (wholeSlice nodes) topo
  mkSlice :: Integral n => Set.Set n -> Slice
  mkSlice nodes = Slice (getAllIntLocs nodes)
                        (Map.fromList $ zip (Set.toList $ getAllHostLocs nodes)
                                            (repeat top))
                        (Map.fromList $ zip (Set.toList $ getAllHostLocs nodes)
                                            (repeat top))
  wholeSlice nodes = Set.map fromIntegral $ Set.union switches hosts where
    switches = nodes
    hosts = Set.unions . Set.toList .
            Set.map (\n -> Set.fromList [100 + 10 * n, 101 + 10 * n]) $
            nodes
  getAllIntLocs :: Integral n => Set.Set n -> Set.Set Loc
  getAllIntLocs = Set.unions . map getIntLocs . map fromIntegral . Set.toList
  getIntLocs n = if n == 0 then Set.singleton (Loc n 2)
                 else if n == fromIntegral max then Set.singleton (Loc n 1)
                 else Set.fromList [Loc n 1, Loc n 2]
  getAllHostLocs :: Integral n => Set.Set n -> Set.Set Loc
  getAllHostLocs = Set.unions . map getHostLocs . map fromIntegral . Set.toList
  getHostLocs n = Set.fromList [Loc n 3, Loc n 4]

-- |Construct a k-complete graph and k slices on floor(k/2) nodes
kComplete :: Integral k => k -> (Topo, [([Node], Slice, Policy)])
kComplete k = (topo, map mkCombined sliceNodes) where
  k' = fromIntegral k
  topo = TG.kComplete k'
  length = k' `quot` 2
  -- [0, 1, 2], [1, 2, 3], ..., [4, 5, 6], [5, 6, 0], [6, 0, 1]
  sliceNodes = take k' . map (take length) . List.tails . cycle $ [0 .. k']
  mkCombined nodes = (nodes, slice, policy) where
    topo' = subgraph (Set.fromList nodes) topo
    slice = internalSlice topo'
    policy = simuFlood topo'

-- |Build a linear graph from input slices.  Requires as many queries as slices
linearQ :: Integral n => [[n]] -> [Query]-> (Topo, [Policy])
linearQ nodess queries = (topo, policies) where
  nodess' = map Set.fromList nodess
  max = maximum . concat . map Set.toList $ nodess'
  topo = TG.linear (max + 1)
  policies = map mkPolicy (zip nodess' queries)
  mkPolicy (nodes, q) =
    simuFloodQuery (subgraph (Set.map fromIntegral nodes) topo) q

-- |Build a linear graph from input slices with two hosts per node.  Requires as
-- many queries as slices
linearHostsQ :: Integral n => [[n]] -> [Query]-> (Topo, [(Slice, Policy)])
linearHostsQ nodess queries = (topo, map (\(ns, q) -> (mkSlice ns, mkPolicy ns q))
                                        (zip nodess' queries)) where
  nodess' = map Set.fromList nodess
  max = maximum . concat . map Set.toList $ nodess'
  topo = TG.linearHosts (max + 1)
  mkPolicy nodes q = simuFloodQuery (subgraph (wholeSlice nodes) topo) q
  mkSlice :: Integral n => Set.Set n -> Slice
  mkSlice nodes = Slice (getAllIntLocs nodes)
                        (Map.fromList $ zip (Set.toList $ getAllHostLocs nodes)
                                            (repeat top))
                        (Map.fromList $ zip (Set.toList $ getAllHostLocs nodes)
                                            (repeat top))
  wholeSlice nodes = Set.map fromIntegral $ Set.union switches hosts where
    switches = nodes
    hosts = Set.unions . Set.toList .
            Set.map (\n -> Set.fromList [100 + 10 * n, 101 + 10 * n]) $
            nodes
  getAllIntLocs :: Integral n => Set.Set n -> Set.Set Loc
  getAllIntLocs = Set.unions . map getIntLocs . map fromIntegral . Set.toList
  getIntLocs n = if n == 0 then Set.singleton (Loc n 2)
                 else if n == fromIntegral max then Set.singleton (Loc n 1)
                 else Set.fromList [Loc n 1, Loc n 2]
  getAllHostLocs :: Integral n => Set.Set n -> Set.Set Loc
  getAllHostLocs = Set.unions . map getHostLocs . map fromIntegral . Set.toList
  getHostLocs n = Set.fromList [Loc n 3, Loc n 4]

-- |Construct a k-complete graph and k slices on floor(k/2) nodes.  Requires k
-- queries
kCompleteQ :: Integral k => k -> [Query] -> (Topo, [([Node], Slice, Policy)])
kCompleteQ k queries = (topo, map mkCombined $ zip sliceNodes queries) where
  k' = fromIntegral k
  topo = TG.kComplete k'
  length = k' `quot` 2
  -- [0, 1, 2], [1, 2, 3], ..., [4, 5, 6], [5, 6, 0], [6, 0, 1]
  sliceNodes = take k' . map (take length) . List.tails . cycle $ [1 .. k']
  mkCombined (nodes, q) = (nodes, slice, policy) where
    topo' = subgraph (Set.fromList nodes) topo
    slice = internalSlice topo'
    policy = simuFloodQuery topo' q

-- |Construct a k-complete graph and k slices on floor(k/2) nodes with two hosts
-- on each switch.  Each slice is assigned the IP address of its lowest member.
-- Requires k queries.
kCompleteQHosts :: Integral k => k -> [Query] ->
                                 (Topo, [([Node], Slice, Policy)])
kCompleteQHosts k queries = (topo, map mkCombined $ zip sliceNodes queries) where
  k' = fromIntegral k
  topo = TG.kCompleteHosts k'
  length = k' `quot` 2
  -- [1, 2, 3], ..., [4, 5, 6], [5, 6, 1], [6, 1, 2]
  sliceNodes = take k' .
               map (take length) .
               List.tails .
               cycle $ [1 .. k']
  mkCombined (nodes, q) = (nodes, slice, policy) where
    topo' = subgraph (Set.fromList . map fromIntegral $ nodes) topo
    edges = concat .
            map (\i -> [ Loc i (fromIntegral $ 101 + 10 * i)
                       , Loc i (fromIntegral $ 102 + 10 * i)]) .
            map fromIntegral $
            nodes
    min = fromIntegral $ minimum nodes
    inbound  = Map.fromList . map (\l -> (l, nwDst min)) $ edges
    outbound = Map.fromList . map (\l -> (l, nwSrc min)) $ edges
    slice = (internalSlice topo') {ingress = inbound, egress = outbound}
    policy = simuFloodQuery topo' q
