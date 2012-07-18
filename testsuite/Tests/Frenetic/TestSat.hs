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
  assertBool "topology transfers" (result)

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

basicSlice = Slice (Set.fromList [ Loc 1 1, Loc 1 2
                                 , Loc 2 1, Loc 2 2, Loc 2 3
                                 ]) Map.empty Map.empty

case_testIsForwards = do
  let o = (PrTo 2) ==> forward 1
  let r = (PrTo 2) ==> forward 1
  result <- checkBool $ simulatesForwards basicSlice o r
  assertBool "forward identically" (not result)

  let o = (PrTo 2) <&> (inPort 2) ==> forward 1
  let r = (PrTo 2) <&> (inPort 2) ==> forward 1
  result <- checkBool $ simulatesForwards basicSlice o r
  assertBool "forward identically" (not result)
  result <- checkBool $ simulatesForwards basicSlice r o
  assertBool "forward identically" (not result)

  let o = (PrTo 2) <&> (inPort 2) ==> forward 1
  let r = (PrTo 2) <&> (inPort 2) ==> forwardMods [(1, patDlVlan 2)]
  result <- checkBool $ simulatesForwards basicSlice o r
  assertBool "forward identically" (not result)
  result <- checkBool $ simulatesForwards basicSlice r o
  assertBool "forward identically" (not result)

  let o = (PrTo 2) <&> (PrPattern (top { ptrnInPort = Exact 1
                                       , ptrnDlSrc = Exact 32432
                                       , ptrnDlDst = Exact 324322}))
          ==> forward 1
  let r = (PrTo 2) <&> (PrPattern (top { ptrnInPort = Exact 1
                                       , ptrnDlSrc = Exact 32432
                                       , ptrnDlDst = Exact 324322}))
          ==> forwardMods [(1, patDlVlan 2)]
  result <- checkBool $ simulatesForwards basicSlice o r
  assertBool "forward identically" (not result)
  result <- checkBool $ simulatesForwards basicSlice r o
  assertBool "forward identically" (not result)
