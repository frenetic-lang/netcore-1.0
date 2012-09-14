module Tests.Frenetic.TestSat where

import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.Framework.Providers.HUnit
import Tests.Frenetic.Util
import Frenetic.Pattern
import Frenetic.NetCore
import Frenetic.NetCore.Pretty
import Frenetic.NetCore.Short
import Frenetic.Sat
import Frenetic.Slices.Sat
import Frenetic.Slices.Slice
import Frenetic.Topo
import Frenetic.NetCore.Types
import Frenetic.Z3

import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set

satTests = $(testGroupGenerator)

dlVlan v = DlVlan (Just v)

nwDst x = NwDst (IPAddressPrefix (IPAddress x) 32)

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
  (_, query1) <- countPkts 1
  (_, query2) <- countPkts 1 -- different query ID
  let o = Switch 2 ==> query1
  let r = Switch 2 ==> query2
  result <- checkBool $ breaksObserves topo Nothing o o
  assertBool "identical observations good." (not result)
  result <- checkBool $ breaksObserves topo Nothing o r
  assertBool "separate observations bad." (result)
  let o = Any ==> query1
  let r = (Switch 2) ==> query1
  result <- checkBool $ breaksObserves topo Nothing o r
  assertBool "different predicates bad." (result)

case_testBreaksForwards = do
  let o = (Switch 2) ==> forward [1]
  let r = (Switch 2) ==> forward [1]
  result <- checkBool $ breaksForwards topo Nothing o r
  assertBool "identical switch" (not result)

  let o = (Switch 2) <&&> (IngressPort 2) ==> forward [1]
  let r = (Switch 2) <&&> (IngressPort 2) ==> forward [1]
  result <- checkBool $ breaksForwards topo Nothing o r
  assertBool "identical locations" (not result)

  let o = (Switch 2) <&&> (IngressPort 2) ==> forward [1]
  let r = (Switch 2) <&&> (IngressPort 2) ==> modify [(1, modDlVlan (Just 2))]
  result <- checkBool $ breaksForwards topo Nothing o r
  assertBool "match vlans" (not result)
  result <- checkBool $ breaksForwards topo Nothing r o
  assertBool "match vlans rev" (not result)

  let o = Switch 2 <&&> IngressPort 1
                   <&&> DlSrc (EthernetAddress 32432)
                   <&&> DlDst (EthernetAddress 324322)
          ==> forward [1]
  let r = Switch 2 <&&> IngressPort 1
                     <&&> DlSrc (EthernetAddress 32432)
                     <&&> DlDst (EthernetAddress 324322)
          ==> modify [(1, modDlVlan (Just 2))]
  result <- checkBool $ breaksForwards topo Nothing o r
  assertBool "set vlans" (not result)
  result <- checkBool $ breaksForwards topo Nothing r o
  assertBool "set vlans rev" (not result)

case_testBreaksForwardsFlood = do
  let topo' = buildGraph [ ((1, 0), (9, 1))
                         , ((2, 0), (9, 2))
                         , ((3, 0), (9, 3))
                         ]
  let o = Switch 9 ==> allPorts unmodified
  let r = (inport 9 1 ==> forward [2, 3]) <+>
          (inport 9 2 ==> forward [1, 3]) <+>
          (inport 9 3 ==> forward [1, 2])
  result <- checkBool $ breaksForwards topo' Nothing o r
  assertBool "flood semantics" (not result)

case_testBreaksForwards2 = do
  let o = ((Switch 1) ==> forward [1]) <+> ((Switch 2) ==> forward [2])
  result <- checkBool $ breaksForwards2 topo Nothing o o
  assertBool "identical switch" (not result)

  let o = ((Switch 1) ==> forward [1]) <+> ((Switch 2) ==> forward [2])
  let r = ((Switch 1) ==> modify [(1, modDlVlan (Just 2))]) <+>
          ((Switch 2) ==> modify [(2, modDlVlan (Just 3))])
  result <- checkBool $ breaksForwards2 topo Nothing o r
  assertBool "match vlans" (not result)
  result <- checkBool $ breaksForwards2 topo Nothing r o
  assertBool "match vlans rev" (not result)

case_testForwardsRestriction = do
  let o = Any ==> allPorts unmodified
  let r = (inport 2 1 ==> forward [2]) <+>
          (inport 2 2 ==> forward [1])
  result <- checkBool $ breaksForwards topo (Just basicSlice) o r
  assertBool "restriction works" (not result)
  result <- checkBool $ breaksForwards topo Nothing r o
  assertBool "flood encompasses forwarding" (not result)

case_testOneVlanPerEdge = do
  let pol = (dlVlan 1) ==> forward [1, 1]
  result <- checkBool $ multipleVlanEdge topo pol
  assertBool "multipleVlanEdge allows functioning programs" (not result)
  let pol = Any ==> modify [(1, modDlVlan (Just 1)), (1, modDlVlan (Just 2))]
  result <- checkBool $ multipleVlanEdge topo pol
  assertBool "multipleVlanEdge spots problems" result

case_testDomain = do
  let pol = inport 2 1 ==> forward [2]
  result <- checkBool $ unconfinedDomain topo basicSlice pol
  assertBool "unconfinedDomain false on confined" (not result)
  let pol = Switch 3 ==> forward [1]
  result <- checkBool $ unconfinedDomain topo basicSlice pol
  assertBool "unconfinedDomain true on unconfined" result

case_testRange = do
  let pol = inport 2 1 ==> forward [2]
  result <- checkBool $ unconfinedRange topo basicSlice pol
  assertBool "unconfinedRange false on confined" (not result)
  let pol = inport 3 1 ==> forward [2]
  result <- checkBool $ unconfinedRange topo basicSlice pol
  assertBool "unconfinedRange true on unconfined" result

case_testCompiledCorrectly = do
  let o = (inport 2 2) <&&> (dlVlan 2) ==> forward [1]
  result <- compiledCorrectly smallTopo smallSlice o o
  assertBool "id compiled correctly" result

  let o = (inport 2 2) <&&> (dlVlan 2) ==> forward [1]
  let r = (inport 2 2) <&&> (dlVlan 2) ==> forward [2]
  result <- compiledCorrectly smallTopo smallSlice o r
  assertBool "basic compiled wrong" (not result)

  let o = (inport 2 2)                ==> forward [1]
  let r = (inport 2 2) <&&> (dlVlan 2) ==> forward [1]
  result <- compiledCorrectly smallTopo smallSlice o r
  assertBool "match vlans in compilation" result

  let o = (inport 2 2)                ==> forward [1]
  let r = (inport 2 2)  ==> modify [(1, modDlVlan (Just 2))]
  result <- compiledCorrectly smallTopo smallSlice o r
  assertBool "set vlans in compilation" result

  (_, q) <- countPkts 1
  let o = (inport 2 2)                 ==> (forward [1] <+> q)
  let r = (inport 2 2) <&&> (dlVlan 2) ==> (forward [1] <+> q)
  result <- compiledCorrectly smallTopo smallSlice o r
  assertBool "queries match" result

case_testBadCompile = do
  let slice = Slice (Set.fromList [Loc 2 2, Loc 3 1])
                    (Map.fromList [(Loc 2 1, nwDst 80)])
                    (Map.fromList [(Loc 3 2, nwDst 80)])
  let o = inport 2 1 ==> forward [2]
  let r = (inport 2 1) <&&> nwDst 80 ==> forward [2]
  result <- compiledCorrectly topo slice o r
  assertBool "grabs too many packets" (not result)

  let o = inport 3 1 ==> forward [2]
  let r = inport 3 1 <&&> nwDst 80 ==> forward [2]
  result <- compiledCorrectly topo slice o r
  assertBool "emits vlan traffic" (not result)

  (_, q1) <- countPkts 1
  (_, q2) <- countPkts 1
  let o = (inport 2 2)                 ==> (forward [1] <+> q1)
  let r = (inport 2 2) <&&> (dlVlan 2) ==> (forward [1] <+> q2)
  result <- compiledCorrectly smallTopo smallSlice o r
  assertBool "queries mismatch" (not result)

case_testInOutRestriction = do
  let slice = Slice (Set.fromList [Loc 2 2, Loc 3 1])
                    (Map.fromList [(Loc 2 1, nwDst 80)])
                    (Map.fromList [(Loc 3 2, nwDst 80)])
  let o = inport 2 1 ==> forward [2]
  let r = (inport 2 1) <&&> nwDst 80 <&&> DlVlan Nothing ==> forward [2]
  result <- compiledCorrectly topo slice o r
  assertBool "lifts into vlan" result

  let o = inport 3 1 ==> forward [2]
  let r = inport 3 1 <&&> nwDst 80 ==> modify [(2, modDlVlan Nothing)]
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
  let p1 = inport 0 1 <&&> DlVlan Nothing ==> forward [2]
  let p2 = inport 0 2 <&&> DlVlan Nothing ==> forward [2]
  result <- checkBool $ sharedInput p1 p2
  assertBool "Disjoint policies have disjoint inputs" (not result)
  let p1 = inport 0 1 <&&> DlVlan Nothing ==> forward [2]
  result <- checkBool $ sharedInput p1 p1
  assertBool "Identical policies have joint inputs" result

case_testSharedOutput = do
  let p1 = inport 0 1 <&&> DlVlan Nothing ==> forward [1]
  let p2 = inport 0 1 <&&> DlVlan Nothing ==> forward [2]
  result <- checkBool $ sharedOutput p1 p2
  assertBool "Disjoint policies have disjoint outputs" (not result)
  let p1 = inport 0 1 <&&> DlVlan Nothing ==> forward [2]
  result <- checkBool $ sharedOutput p1 p1
  assertBool "Identical policies have joint outputs" result
