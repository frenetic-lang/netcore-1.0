module Tests.Frenetic.TestSat where

import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.Framework.Providers.HUnit

import Frenetic.NetCore
import Frenetic.NetCore.Short
import Frenetic.Sat
import Frenetic.Slices.Sat
import Frenetic.Slices.Slice
import Frenetic.Z3
import Frenetic.Topo

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

topo = buildGraph [ ((1, 1), (2, 1))
                  , ((2, 2), (3, 1))
                  , ((3, 2), (4, 1))
                  ]

basicSlice = Slice (Set.fromList [ Loc 1 1
                                 , Loc 2 1, Loc 2 2
                                 , Loc 3 1
                                 ]) Map.empty Map.empty

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

case_testBreaksForwards2 = do
  let o = ((PrTo 1) ==> forward 1) <+> ((PrTo 2) ==> forward 2)
  let r = ((PrTo 1) ==> forward 1) <+> ((PrTo 2) ==> forward 2)
  result <- checkBool $ breaksForwards topo Nothing o r
  assertBool "identical switch" (not result)

  let o = ((PrTo 1) ==> forward 1) <+> ((PrTo 2) ==> forward 2)
  let r = ((PrTo 1) ==> forwardMods [(1, patDlVlan 2)]) <+>
          ((PrTo 2) ==> forwardMods [(2, patDlVlan 3)])
  result <- checkBool $ breaksForwards topo Nothing o r
  assertBool "match vlans" (not result)
  result <- checkBool $ breaksForwards topo Nothing r o
  assertBool "match vlans rev" (not result)

case_testForwardsRestriction = do
  let o = top ==> Action (MS.singleton (PhysicalFlood, top)) []
  let r = (PrTo 1 ==> forward 1) <+>
          (PrTo 2 ==> forwardMods [(1, top), (2, top)]) <+>
          (PrTo 3 ==> forward 1)
  result <- checkBool $ breaksForwards topo (Just basicSlice) o r
  assertBool "restriction works" (not result)
  result <- checkBool $ breaksForwards topo Nothing r o
  assertBool "flood encompasses forwarding" (not result)

  let o = top ==> Action (MS.singleton (PhysicalFlood, top)) []
  let r = (PrTo 1 ==> forward 1) <+>
          (PrTo 2 ==> Action (MS.fromList [(Physical 1, top),
                                           (Physical 2, top)]) []) <+>
          (PrTo 3 ==> forward 2)
  result <- checkBool $ breaksForwards topo (Just basicSlice) o r
  assertBool "restriction failure works" result
  result <- checkBool $ breaksForwards topo (Just basicSlice) r o
  assertBool "restricted flood works" (not result)

case_testOneVlanPerEdge = do
  let pol = (dlVlan 1) ==> forwardMods [(1, top), (1, top)]
  result <- checkBool $ multipleVlanEdge topo pol
  assertBool "multipleVlanEdge allows functioning programs" (not result)
  let pol = top ==> forwardMods [(1, patDlVlan 1), (1, patDlVlan 2)]
  result <- checkBool $ multipleVlanEdge topo pol
  assertBool "multipleVlanEdge spots problems" result

case_testDomain = do
  let pol = inport 2 1 ==> forward 2
  result <- checkBool $ unconfinedDomain basicSlice pol
  assertBool "unconfinedDomain false on confined" (not result)
  let pol = PrTo 3 ==> forward 1
  result <- checkBool $ unconfinedDomain basicSlice pol
  assertBool "unconfinedDomain true on unconfined" result

case_testRange = do
  let pol = inport 2 1 ==> forward 2
  result <- checkBool $ unconfinedRange basicSlice pol
  assertBool "unconfinedRange false on confined" (not result)
  let pol = inport 3 1 ==> forward 2
  result <- checkBool $ unconfinedRange basicSlice pol
  assertBool "unconfinedRange true on unconfined" result
