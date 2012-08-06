module Tests.Frenetic.Slices.TestEndToEnd where

import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Test.HUnit hiding (Node)
import Test.Framework.Providers.HUnit
import Frenetic.Pattern hiding (intersect)
import Tests.Frenetic.Util
import Frenetic.NetCore
import Frenetic.NetCore.Pretty
import Frenetic.NetCore.Reduce
import Frenetic.NetCore.Types
import Frenetic.PolicyGen
import Frenetic.Slices.Compile
import Frenetic.Slices.Slice
import Frenetic.Slices.Sat
import Frenetic.Slices.VlanAssignment
import Frenetic.Topo
import Frenetic.Z3

import Data.Graph.Inductive.Graph
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.MultiSet as MS
import qualified Data.Set as Set

endToEndTests = $(testGroupGenerator)

case_testBasicCompile = do
  qs <- queries
  let (topo, [p1, p2]) = linearQ [[0, 1, 2, 3], [0, 1, 2, 3]] qs
  let slice = internalSlice topo
  let c1 = compileSlice slice 1 p1
  let c2 = compileSlice slice 2 p2
  result <- separate topo p1 p2
  assertBool "Initials not separate" (not result)
  result <- separate topo c1 c2
  assertBool "Compiled separate" result
  result <- compiledCorrectly topo slice p1 c1
  assertBool "Compiled correctly" result

case_testFloodCompile = do
  qs <- queries
  let topo = buildGraph [ ((1, 0), (9, 1))
                        , ((2, 0), (9, 2)) ]
  let p = PrTo 9 ==> allPorts unmodified
  let slice = Slice Set.empty (Map.fromList [(Loc 9 1, top), (Loc 9 2, top)])
                              (Map.fromList [(Loc 9 1, top), (Loc 9 2, top)])
  let c = compileSlice slice 1 p
  result <- compiledCorrectly topo slice p c
  assertBool "Compiled correctly" result

case_testBasicEdge = do
  qs <- queries
  let (topo, [p1, p2]) = linearQ [[0, 1, 2, 3], [0, 1, 2, 3]] qs
  let slice = internalSlice topo
  let [(a1, (s1, _)), (a2, (s2, _))] = edge topo [(slice, p1), (slice, p2)]
  let c1 = edgeCompileSlice s1 a1 p1
  let c2 = edgeCompileSlice s2 a2 p2
  result <- separate topo c1 c2
  assertBool "Compiled separate" result
  result <- compiledCorrectly topo slice p1 c1
  assertBool "Compiled correctly" result

case_testBasicWithHosts = do
  qs <- queries
  let (topoNoHosts, _) = linear [[0, 1, 2, 3], [0, 1, 2, 3]]
  let (topo, [(_, p1), (_, p2)]) = linearHostsQ [[0, 1, 2, 3], [0, 1, 2, 3]] qs
  let slice = internalSlice topoNoHosts
  let c1 = compileSlice slice 1 p1
  let c2 = compileSlice slice 2 p2
  result <- separate topo p1 p2
  assertBool "Initials not separate" (not result)
  result <- separate topo c1 c2
  assertBool "Compiled separate" result
  result <- compiledCorrectly topo slice p1 c1
  assertBool "Compiled correctly" result

case_testBasicEdgeWithHosts = do
  qs <- queries
  let (topoNoHosts, _) = linear [[0, 1, 2, 3], [0, 1, 2, 3]]
  let (topo, [(_, p1), (_, p2)]) = linearHostsQ [[0, 1, 2, 3], [0, 1, 2, 3]] qs
  let slice = internalSlice topoNoHosts
  let [(a1, (s1, _)), (a2, (s2, _))] = edge topo [(slice, p1), (slice, p2)]
  let c1 = edgeCompileSlice s1 a1 p1
  let c2 = edgeCompileSlice s1 a2 p2
  result <- separate topo c1 c2
  assertBool "Compiled separate" result
  result <- compiledCorrectly topo slice p1 c1
  assertBool "Compiled correctly" result

case_testHostsCompile = do
  qs <- queries
  let (topo, [(s1, p1), (s2, p2)]) = linearHostsQ [[0, 1, 2, 3], [0, 1, 2, 3]]
                                                  qs
  let c1 = compileSlice s1 1 p1
  let c2 = compileSlice s2 2 p2
  result <- separate topo p1 p2
  assertBool "Initials not separate" (not result)
  result <- checkBool $ sharedIO topo c1 c2
  assertBool "Compiled separate up to edges" (not result)
  result <- separate topo c1 c2
  assertBool "Compiled not separate" (not result)
  result <- compiledCorrectly topo s1 p1 c1
  assertBool "Compiled correctly" result

case_testHostsCompileEdge = do
  qs <- queries
  let (topo, [(s1, p1), (s2, p2)]) = linearHostsQ [[0, 1, 2, 3], [0, 1, 2, 3]]
                                                  qs
  let [(a1, (s1', _)), (a2, (s2', _))] = edge topo [(s1, p1), (s2, p2)]
  let c1 = edgeCompileSlice s1' a1 p1
  let c2 = edgeCompileSlice s2' a2 p2
  result <- checkBool $ sharedIO topo c1 c2
  assertBool "Compiled separate up to edges" (not result)
  result <- separate topo c1 c2
  assertBool "Compiled not separate" (not result)
  result <- compiledCorrectly topo s1 p1 c1
  assertBool "Compiled correctly" result

case_testInputPredicates = do
  qs <- queries
  let (topo, [(s1, p1), (s2, p2)]) = linearHostsQ [[0, 1, 2, 3], [0, 1, 2, 3]] qs
  let s1' = s1 {ingress = Map.map (\_ -> dlSrc 100) (ingress s1)}
  let s2' = s2 {ingress = Map.map (\_ -> dlSrc 200) (ingress s2)}
  let c1 = compileSlice s1' 1 p1
  let c2 = compileSlice s2' 2 p2
  result <- checkBool $ sharedIO topo c1 c2
  assertBool "Compiled separate up to edges" (not result)
  result <- checkBool $ sharedInput c1 c2
  assertBool "Compiled do not share inputs" (not result)
  result <- separate topo c1 c2
  assertBool "Compiled not separate" (not result)
  result <- compiledCorrectly topo s1' p1 c1
  assertBool "Compiled correctly" result

case_testInputPredicatesEdge = do
  qs <- queries
  let (topo, [(s1, p1), (s2, p2)]) = linearHostsQ [[0, 1], [0, 1]] qs
  let s1' = s1 {ingress = Map.map (\_ -> dlSrc 100) (ingress s1)}
  let s2' = s2 {ingress = Map.map (\_ -> dlSrc 200) (ingress s2)}
  let [(a1, (_, _)), (a2, (_, _))] = edge topo [(s1', p1), (s2', p2)]
  let c1 = edgeCompileSlice s1' a1 p1
  let c2 = edgeCompileSlice s2' a2 p2
  result <- checkBool $ sharedIO topo c1 c2
  assertBool "Compiled separate up to edges" (not result)
  result <- checkBool $ sharedInput c1 c2
  assertBool "Compiled do not share inputs" (not result)
  result <- separate topo c1 c2
  assertBool "Compiled not separate" (not result)
  result <- compiledCorrectly topo s1' p1 c1
  assertBool "Compiled correctly" result

-- Here begins the heavyweight tests on autogenerated complete graphs.  These
-- are slow, and can take a few minutes to run.
buildk6 qs = (topo, map build combined') where
  (topo, combined) = kCompleteQ 6 qs
  combined' = zip combined [1..]
  build ((ns, s, p), vl) = (ns, s, p, compileSlice s vl p)

buildk6Hosts qs = (topo, map build combined') where
  (topo, combined) = kCompleteQHosts 6 qs
  combined' = zip combined [1..]
  build ((ns, s, p), vl) = (ns, s, p, compileSlice s vl p)

buildk6Edge qs = (topo, map build vls) where
  (topo, combined) = kCompleteQ 6 qs
  combined' = map (\ (_, s, p) -> (s, p)) combined
  vls = edge topo combined'
  build ((a, (s, p))) = (ns, s, p, edgeCompileSlice s a p) where
    ns = sort . nub . map (\(Loc s _) -> fromIntegral s) . Set.toList $
         internal s

-- |[1, 2, 3] -> [(1, 2), (1, 3), (2, 3)]
getPairs :: [a] -> [(a, a)]
getPairs as = [(x,y) | (x:xs) <- tails as, y <- xs]

case_testK6PhysSep = do
  qs <- queries
  let (k6topo, k6combined) = buildk6 qs
  let nodePolicies = map (\ (ns, _, p, _) -> (ns, p)) k6combined
  physSepReflex k6topo nodePolicies
  physSep k6topo nodePolicies

case_testK6Compile = do
  qs <- queries
  let (k6topo, k6combined) = buildk6 qs
  compileReflex k6topo k6combined
  compile k6topo k6combined

case_testK6CompileHosts = do
  qs <- queries
  let (k6topo, k6combined) = buildk6Hosts qs
  compileReflex k6topo k6combined
  compileHosts k6topo k6combined

case_testK6EdgeCompile = do
  qs <- queries
  let (k6topo, k6combined) = buildk6Edge qs
  compileReflex k6topo k6combined
  compile k6topo k6combined

case_testK6EdgeCompileHosts = do
  qs <- queries
  let (k6topo, k6combined) = buildk6Edge qs
  compileReflex k6topo k6combined
  compileHosts k6topo k6combined

-- All the following test operators have a "reflexive" version for testing
-- properties of just one policy and a regular version that tests pairwise
-- properties.  This speeds up testing dramatically compared to testing unary
-- properties within the pairwise test.

physSepReflex :: Topo -> [([Node], Policy)] -> Assertion
physSepReflex topo nodePolicies = sequence_ $ map test nodePolicies where
  test (ns, p) = do
    let label = "not self-isolated: " ++ show ns
    separated <- separate topo p p
    assertBool ("not self-isolated: " ++ show ns) (not separated)

physSep :: Topo -> [([Node], Policy)] -> Assertion
physSep topo nodePolicies = sequence_ $ map test policyPairs where
  policyPairs = getPairs nodePolicies
  test ((ns1, p1), (ns2, p2)) = do
    let label = "physical separation of " ++ show ns1 ++ " and " ++ show ns2
    separated <- separate topo p1 p2
    if length (intersect ns1 ns2) > 1 then
      assertBool ("Should be false: " ++ label) (not separated)
    else
      assertBool ("Should be true: " ++ label) separated

-- No need to repeat compilation correctness testing, so do it in the unary test
-- version.
compileReflex :: Topo -> [([Node], Slice, Policy, Policy)] -> Assertion
compileReflex topo combined = sequence_ $ map test combined where
  test (ns, s, p, c) = do
    correct <- compiledCorrectly topo s p c
    assertBool ("correct compilating of: " ++ show ns) correct
    separated <- separate topo c c
    assertBool ("not self-separate of: " ++ show ns) (not separated)

compile :: Topo -> [([Node], Slice, Policy, Policy)] -> Assertion
compile topo combined = sequence_ $ map test pairs where
  pairs = getPairs combined
  test ((ns1, s1, p1, c1), (ns2, s2, p2, c2)) = do
    separated <- separate topo c1 c2
    assertBool ("separate compilation of: " ++ show ns1 ++ " and " ++ show ns2)
               separated

-- Need a separate test here because we don't have egress predicates.
compileHosts :: Topo -> [([Node], Slice, Policy, Policy)] -> Assertion
compileHosts topo combined = sequence_ $ map test pairs where
  pairs = getPairs combined
  test ((ns1, s1, p1, c1), (ns2, s2, p2, c2)) = do
    if ns1 == [1, 2, 3] &&  ns2 == [2, 3, 4] then do
      io1 <- fmap not . fmap isJust $ sharedIO topo c1 c2
      io2 <- fmap not . fmap isJust $ sharedIO topo c2 c1
      in1 <- fmap not . fmap isJust $ sharedInput c1 c2
      in2 <- fmap not . fmap isJust $ sharedInput c2 c1
      assertBool ("internal separation post compile of: " ++ show ns1 ++
                                                  " and " ++ show ns2)
                 (io1 && io2)
      assertBool ("input separation post compile of: " ++ show ns1 ++
                                               " and " ++ show ns2)
                 (in1 && in2)
    else return ()
