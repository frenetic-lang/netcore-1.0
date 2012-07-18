module Tests.Frenetic.Slices.TestCompile where

import Frenetic.NetCore.Semantics
import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.Framework.Providers.HUnit

import Frenetic.NetCore
import Frenetic.NetCore.Short

import Frenetic.Slices.Compile
import Frenetic.Slices.Slice

import Data.MultiSet as MS

sliceCompileTests = $(testGroupGenerator)

-- Construct a bunch of basically meaningless objects for testing

a1 = Action (MS.fromList [ (Physical 1, patDlSrc 10)
                         , (Physical 2, patDlSrc 10)
                         , (Physical 3, patDlSrc 10)])
            []
a2 = Action (MS.fromList [ (Physical 2, patDlDst 20)
                         , (Physical 3, patDlDst 20)])
            []
a3 = Action (MS.fromList [ (Physical 2, patDlTyp 30)])
            []
a4 = Action (MS.fromList [ (Physical 4, patNwSrc 40)
                         , (Physical 5, patNwSrc 40)
                         , (Physical 6, patNwSrc 40)])
            []
a5 = Action (MS.fromList [ (Physical 3, patNwSrc 50)
                         , (Physical 4, patNwSrc 50)
                         , (Physical 5, patNwSrc 50)
                         , (Physical 6, patNwSrc 50)])
            []

pr1 = inport 1 0
pr2 = inport 1 0 <|> inport 2 3
pr3 = inport 3 3 <&> PrPattern (patDlSrc 10)
pr4 = pr3 <&> neg (PrPattern (patDlDst  20))

po1 = pr1 ==> a1
po2 = pr2 ==> a2
po3 = pr3 ==> a3
po4 = pr4 ==> a4
po5 = pr1 <&> pr4 ==> a5

bigPolicy = ((po3 <+> po4 <+> po5) % pr2) <+> po1 <+> po2
baseForwards = forwardsOfPolicy bigPolicy

forwardsOfPolicy PoBottom        = MS.empty
forwardsOfPolicy (PoBasic _ a)   = forwardsOfAction a
forwardsOfPolicy (PoUnion p1 p2) = MS.union (forwardsOfPolicy p1)
                                          (forwardsOfPolicy p2)

forwardsOfAction (Action ms _) = ms

case_testModifyVlan = do
  let expected = MS.map (\ (port, pat) -> (port, pat {ptrnDlVlan = Exact 1234}))
                        baseForwards
  let observedPolicy = modifyVlan 1234 bigPolicy
  let observedForwards = forwardsOfPolicy observedPolicy
  assertEqual "modifyVlan puts vlan tags on all forwards"
    expected observedForwards

case_testMatchesSwitch = do
  assertBool "pr1 matches switch 1" (matchesSwitch 1 pr1)
  assertBool "pr1 does not match switch 2" (not (matchesSwitch 2 pr1))
  assertBool "pr2 matches switch 1" (matchesSwitch 1 pr2)
  assertBool "pr2 matches switch 2" (matchesSwitch 2 pr2)
  assertBool "pr2 does not match switch 3" (not (matchesSwitch 3 pr2))
  assertBool "pr3 matches switch 3" (matchesSwitch 3 pr3)
  assertBool "pr3 does not match switch 4" (not (matchesSwitch 4 pr3))
  assertBool "pr4 matches switch 3" (matchesSwitch 3 pr4)
  assertBool "pr4 does not match switch 4" (not (matchesSwitch 4 pr4))

case_testSetVlanSimple = do
  let pol = pr1 ==> a3
  let expected = MS.singleton (Physical 2, top { ptrnDlTyp = Exact 30
                                               , ptrnDlVlan = Exact 1234})
  let observed = forwardsOfPolicy $ setVlan 1234 (Loc 1 2) pol
  assertEqual "setVlan set vlan on Loc 1 2" expected observed

case_testSetVlanComplex = do
  let pol = (pr3 ==> a1) <+> (pr3 ==> a5)
  let expected = MS.map (\ (p, m) -> if p == Physical 3
                                   then (p, m {ptrnDlVlan = Exact 1234})
                                   else (p, m))
                    (forwardsOfPolicy pol)
  let observed = forwardsOfPolicy $ setVlan 1234 (Loc 3 3) pol
  assertEqual "setVlan set vlan on Loc 1 3 across union" expected observed
