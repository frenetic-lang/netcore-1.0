module Tests.Frenetic.Slices.TestEndToEnd where

import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.Framework.Providers.HUnit

import Tests.Frenetic.Util

import Frenetic.NetCore
import Frenetic.NetCore.Pretty

import Frenetic.Slices.Compile
import Frenetic.Slices.Slice
import Frenetic.Slices.Sat
import Frenetic.Z3

import qualified Data.Map as Map
import qualified Data.MultiSet as MS

endToEndTests = $(testGroupGenerator)

case_testBasicCompile = do
  let (topo, [p1, p2]) = linear [[0, 1, 2, 3], [0, 1, 2, 3]]
  let slice = internalSlice topo
  let c1 = compileSlice slice 1 p1
  let c2 = compileSlice slice 2 p2
  result <- separate topo p1 p2
  assertBool "Initials not separate" (not result)
  result <- separate topo c1 c2
  assertBool "Compiled separate" result
  result <- compiledCorrectly topo slice p1 c1
  assertBool "Compiled correctly" result

case_testBasicWithHosts = do
  let (topoNoHosts, _) = linear [[0, 1, 2, 3], [0, 1, 2, 3]]
  let (topo, [(_, p1), (_, p2)]) = linearHosts [[0, 1, 2, 3], [0, 1, 2, 3]]
  let slice = internalSlice topoNoHosts
  let c1 = compileSlice slice 1 p1
  let c2 = compileSlice slice 2 p2
  result <- separate topo p1 p2
  assertBool "Initials not separate" (not result)
  result <- separate topo c1 c2
  assertBool "Compiled separate" result
--  result <- compiledCorrectly topo slice p1 c1
--  assertBool "Compiled correctly" result

case_testHostsCompile = do
  let (topo, [(s1, p1), (s2, p2)]) = linearHosts [[0, 1, 2, 3], [0, 1, 2, 3]]
  let c1 = compileSlice s1 1 p1
  let c2 = compileSlice s2 2 p2
  result <- separate topo p1 p2
  assertBool "Initials not separate" (not result)
  result <- checkBool $ sharedIO topo c1 c2
  assertBool "Compiled separate up to edges" (not result)
  result <- separate topo c1 c2
  assertBool "Compiled not separate" (not result)
--  result <- compiledCorrectly topo s1 p1 c1
--  assertBool "Compiled correctly" result

--case_testInputPredicates = do
--  let (topo, [(s1, p1), (s2, p2)]) = linearHosts [[0, 1, 2, 3], [0, 1, 2, 3]]
--  let s1' = s1 {ingress = Map.map (\_ -> dlSrc 100) (ingress s1)}
--  let s2' = s2 {ingress = Map.map (\_ -> dlSrc 200) (ingress s2)}
--  let c1 = compileSlice s1' 1 p1
--  let c2 = compileSlice s2' 2 p2
--  result <- checkBool $ sharedIO topo c1 c2
--  assertBool "Compiled separate up to edges" (not result)
--  result <- checkBool $ sharedInput c1 c2
--  assertBool "Compiled do not share inputs" (not result)
--  putStrLn "c1:"
--  putStrLn $ show c1
--  putStrLn "c2:"
--  putStrLn $ show c2
--  result <- separate topo c1 c2
--  assertBool "Compiled not separate" (not result)
--  result <- compiledCorrectly topo s1' p1 c1
--  assertBool "Compiled correctly" result
