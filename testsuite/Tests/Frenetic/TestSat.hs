module Tests.Frenetic.TestSat where

import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.Framework.Providers.HUnit

import Tests.Frenetic.Util

import Frenetic.NetCore
import Frenetic.NetCore.Pretty
import Frenetic.NetCore.Short
import Frenetic.Sat
import Frenetic.Slices.Sat
import Frenetic.Slices.Slice
import Frenetic.Topo
import Frenetic.Z3

import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.MultiSet as MS

satTests = $(testGroupGenerator)

case_testBasic = do
  let t = Input [] [] [ZTrue]
  result <- check t
  (Just "(model \n)") @=? result
  let f = Input [] [] [ZFalse]
  result <- check f
  (Nothing) @=? result

-- 1--2--3--4
topo = buildGraph [ ((1, 1), (2, 1))
                  , ((2, 2), (3, 1))
                  , ((3, 2), (4, 1))
                  ]

smallTopo = buildGraph [ ((1, 1), (2, 1))
                       , ((1, 2), (1, 2)) ]

basicSlice = Slice (Set.fromList [ Loc 1 1
                                 , Loc 2 1, Loc 2 2
                                 , Loc 3 1
                                 ])
                   Map.empty Map.empty

smallSlice = Slice (Set.fromList ([ Loc 1 1, Loc 1 2,
                                    Loc 2 1, Loc 2 2 ]))
                   Map.empty Map.empty

case_testTransfer = do
  let p = Z3Packet "p"
  let p' = Z3Packet "pp"
  let consts = [ DeclConst (ConstPacket p)
               , DeclConst (ConstPacket p')]
  let assertions = [ Equals (switch p)  (Primitive 2)
                   , Equals (port p)    (Primitive 2)
                   , Equals (switch p') (Primitive 3)
                   , Equals (port p')   (Primitive 1)
                   , transfer topo p p'
                   ]
  result <- checkBool . check $ Input setUp consts assertions
  assertBool "topology transfers" result

case_testTransferBreaks = do
  let p = Z3Packet "p"
  let p' = Z3Packet "pp"
  let consts = [ DeclConst (ConstPacket p)
               , DeclConst (ConstPacket p')]
  let assertions = [ Equals (switch p)  (Primitive 2)
                   , Equals (port p)    (Primitive 1)
                   , Equals (switch p') (Primitive 3)
                   , Equals (port p')   (Primitive 1)
                   , transfer topo p p'
                   ]
  result <- checkBool . check $ Input setUp consts assertions
  assertBool "topology does not transfer" (not result)

case_testBreaksObserves = do
  (_, query1) <- query 1
  (_, query2) <- query 1 -- different query ID
  let o = PrTo 2 ==> Action MS.empty [query1]
  let r = PrTo 2 ==> Action MS.empty [query2]
  result <- checkBool $ breaksObserves topo Nothing o o
  assertBool "identical observations good." (not result)
  result <- checkBool $ breaksObserves topo Nothing o r
  assertBool "separate observations bad." (result)

  let o = top ==> Action MS.empty [query1]
  let r = (PrTo 2) ==> Action MS.empty [query1]
  result <- checkBool $ breaksObserves topo Nothing o r
  assertBool "different predicates bad." (result)

case_testBreaksForwards = do
  let o = (PrTo 2) ==> forward 1
  let r = (PrTo 2) ==> forward 1
  result <- checkBool $ breaksForwards topo Nothing o r
  assertBool "identical switch" (not result)

  let o = (PrTo 2) <&> (inPort 2) ==> forward 1
  let r = (PrTo 2) <&> (inPort 2) ==> forward 1
  result <- checkBool $ breaksForwards topo Nothing o r
  assertBool "identical locations" (not result)

  let o = (PrTo 2) <&> (inPort 2) ==> forward 1
  let r = (PrTo 2) <&> (inPort 2) ==> forwardMods [(1, patDlVlan 2)]
  result <- checkBool $ breaksForwards topo Nothing o r
  assertBool "match vlans" (not result)
  result <- checkBool $ breaksForwards topo Nothing r o
  assertBool "match vlans rev" (not result)

  let o = (PrTo 2) <&> (PrPattern (top { ptrnInPort = Exact 1
                                       , ptrnDlSrc = Exact 32432
                                       , ptrnDlDst = Exact 324322}))
          ==> forward 1
  let r = (PrTo 2) <&> (PrPattern (top { ptrnInPort = Exact 1
                                       , ptrnDlSrc = Exact 32432
                                       , ptrnDlDst = Exact 324322}))
          ==> forwardMods [(1, patDlVlan 2)]
  result <- checkBool $ breaksForwards topo Nothing o r
  assertBool "set vlans" (not result)
  result <- checkBool $ breaksForwards topo Nothing r o
  assertBool "set vlans rev" (not result)

case_testBreaksForwardsFlood = do
  let topo' = buildGraph [ ((1, 0), (9, 1))
                         , ((2, 0), (9, 2))
                         , ((3, 0), (9, 3))
                         ]
  let o = PrTo 9 ==> flood
  let r = (inport 9 1 ==> forwardMods [(2, top), (3, top)]) <+>
          (inport 9 2 ==> forwardMods [(1, top), (3, top)]) <+>
          (inport 9 3 ==> forwardMods [(1, top), (2, top)])
  result <- checkBool $ breaksForwards topo' Nothing o r
  assertBool "flood semantics" (not result)

case_testBreaksForwards2 = do
  let o = ((PrTo 1) ==> forward 1) <+> ((PrTo 2) ==> forward 2)
  result <- checkBool $ breaksForwards2 topo Nothing o o
  assertBool "identical switch" (not result)

  let o = ((PrTo 1) ==> forward 1) <+> ((PrTo 2) ==> forward 2)
  let r = ((PrTo 1) ==> forwardMods [(1, patDlVlan 2)]) <+>
          ((PrTo 2) ==> forwardMods [(2, patDlVlan 3)])
  result <- checkBool $ breaksForwards2 topo Nothing o r
  assertBool "match vlans" (not result)
  result <- checkBool $ breaksForwards2 topo Nothing r o
  assertBool "match vlans rev" (not result)

case_testForwardsRestriction = do
  let o = top ==> Action (MS.singleton (PhysicalFlood, top)) []
  let r = (inport 2 1 ==> forward 2) <+>
          (inport 2 2 ==> forward 1)
  result <- checkBool $ breaksForwards topo (Just basicSlice) o r
  assertBool "restriction works" (not result)
  result <- checkBool $ breaksForwards topo Nothing r o
  assertBool "flood encompasses forwarding" (not result)

case_testOneVlanPerEdge = do
  let pol = (dlVlan 1) ==> forwardMods [(1, top), (1, top)]
  result <- checkBool $ multipleVlanEdge topo pol
  assertBool "multipleVlanEdge allows functioning programs" (not result)
  let pol = top ==> forwardMods [(1, patDlVlan 1), (1, patDlVlan 2)]
  result <- checkBool $ multipleVlanEdge topo pol
  assertBool "multipleVlanEdge spots problems" result

case_testDomain = do
  let pol = inport 2 1 ==> forward 2
  result <- checkBool $ unconfinedDomain topo basicSlice pol
  assertBool "unconfinedDomain false on confined" (not result)
  let pol = PrTo 3 ==> forward 1
  result <- checkBool $ unconfinedDomain topo basicSlice pol
  assertBool "unconfinedDomain true on unconfined" result

case_testRange = do
  let pol = inport 2 1 ==> forward 2
  result <- checkBool $ unconfinedRange topo basicSlice pol
  assertBool "unconfinedRange false on confined" (not result)
  let pol = inport 3 1 ==> forward 2
  result <- checkBool $ unconfinedRange topo basicSlice pol
  assertBool "unconfinedRange true on unconfined" result

case_testCompiledCorrectly = do
  let o = (inport 2 2) <&> (dlVlan 2) ==> forward 1
  result <- compiledCorrectly smallTopo smallSlice o o
  assertBool "id compiled correctly" result

  let o = (inport 2 2) <&> (dlVlan 2) ==> forward 1
  let r = (inport 2 2) <&> (dlVlan 2) ==> forward 2
  result <- compiledCorrectly smallTopo smallSlice o r
  assertBool "basic compiled wrong" (not result)

  let o = (inport 2 2)                ==> forward 1
  let r = (inport 2 2) <&> (dlVlan 2) ==> forward 1
  result <- compiledCorrectly smallTopo smallSlice o r
  assertBool "match vlans in compilation" result

  let o = (inport 2 2)                ==> forward 1
  let r = (inport 2 2)  ==> forwardMods [(1, patDlVlan 2)]
  result <- compiledCorrectly smallTopo smallSlice o r
  assertBool "set vlans in compilation" result

  (_, q) <- query 1
  let o = (inport 2 2)                ==> forwardQuery 1 q
  let r = (inport 2 2) <&> (dlVlan 2) ==> forwardQuery 1 q
  result <- compiledCorrectly smallTopo smallSlice o r
  assertBool "queries match" result

case_testBadCompile = do
  let slice = Slice (Set.fromList [Loc 2 2, Loc 3 1])
                    (Map.fromList [(Loc 2 1, nwDst 80)])
                    (Map.fromList [(Loc 3 2, nwDst 80)])
  let o = inport 2 1 ==> forward 2
  let r = (inport 2 1) <&> nwDst 80 ==> forward 2
  result <- compiledCorrectly topo slice o r
  assertBool "grabs too many packets" (not result)

  let o = inport 3 1 ==> forward 2
  let r = inport 3 1 <&> nwDst 80 ==> forward 2
  result <- compiledCorrectly topo slice o r
  assertBool "emits vlan traffic" (not result)

  (_, q1) <- query 1
  (_, q2) <- query 1
  let o = (inport 2 2)                ==> forwardQuery 1 q1
  let r = (inport 2 2) <&> (dlVlan 2) ==> forwardQuery 1 q2
  result <- compiledCorrectly smallTopo smallSlice o r
  assertBool "queries mismatch" (not result)

case_testInOutRestriction = do
  let slice = Slice (Set.fromList [Loc 2 2, Loc 3 1])
                    (Map.fromList [(Loc 2 1, nwDst 80)])
                    (Map.fromList [(Loc 3 2, nwDst 80)])
  let o = inport 2 1 ==> forward 2
  let r = (inport 2 1) <&> nwDst 80 <&> dlVlan 0 ==> forward 2
  result <- compiledCorrectly topo slice o r
  assertBool "lifts into vlan" result

  let o = inport 3 1 ==> forward 2
  let r = inport 3 1 <&> nwDst 80 ==> forwardMods [(2, patDlVlan 0)]
  result <- compiledCorrectly topo slice o r
  assertBool "leaves vlan" result

case_basicOverlap = do
  let (topo, [p1, p2]) = linear [[0, 1, 2], [1, 2, 3]]
  result <- checkBool $ sharedIO topo p1 p2
  assertBool "detects overlap" result

case_basicIso = do
  let (topo, [p1, p2]) = linear [[0, 1], [2, 3]]
  result <- checkBool $ sharedIO topo p1 p2
  assertBool "disjoint nodes isolated" (not result)
  let (topo, [p1, p2]) = linear [[0, 1, 2], [2, 3]]
  result <- checkBool $ sharedIO topo p1 p2
  assertBool "disjoint edges isolated" (not result)

case_testSharedInput = do
  let p1 = inport 0 1 <&> dlVlan 0 ==> forward 2
  let p2 = inport 0 2 <&> dlVlan 0 ==> forward 2
  result <- checkBool $ sharedInput p1 p2
  assertBool "Disjoint policies have disjoint inputs" (not result)
  let p1 = inport 0 1 <&> dlVlan 0 ==> forward 2
  result <- checkBool $ sharedInput p1 p1
  assertBool "Identical policies have joint inputs" result

case_testSharedOutput = do
  let p1 = inport 0 1 <&> dlVlan 0 ==> forward 1
  let p2 = inport 0 1 <&> dlVlan 0 ==> forward 2
  result <- checkBool $ sharedOutput p1 p2
  assertBool "Disjoint policies have disjoint outputs" (not result)
  let p1 = inport 0 1 <&> dlVlan 0 ==> forward 2
  result <- checkBool $ sharedOutput p1 p1
  assertBool "Identical policies have joint outputs" result
