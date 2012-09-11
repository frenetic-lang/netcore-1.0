module Tests.Frenetic.Slices.TestCompile where

import Frenetic.NetCore.Semantics
import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.Framework.Providers.HUnit
import Frenetic.NetCore.Types
import Frenetic.NetCore
import Frenetic.NetCore.Short
import Frenetic.Slices.Compile
import Frenetic.Slices.Slice
import Frenetic.NetCore.Util

sliceCompileTests = $(testGroupGenerator)

-- Construct a bunch of basically meaningless objects for testing

a1 = modify [ (1, modDlSrc $ EthernetAddress 10)
            , (2, modDlSrc $ EthernetAddress 10)
            , (3, modDlSrc $ EthernetAddress 10)]
a2 = modify [ (2, modDlDst $ EthernetAddress 20)
            , (3, modDlDst $ EthernetAddress 20)]
a3 = modify [ (2, modNwDst $ IPAddress 30)]
a4 = modify [ (4, modNwSrc $ IPAddress 40)
            , (5, modNwSrc $ IPAddress 40)
            , (6, modNwSrc $ IPAddress 40)]
a5 = modify [ (3, modNwSrc $ IPAddress 50)
            , (4, modNwSrc $ IPAddress 50)
            , (5, modNwSrc $ IPAddress 50)
            , (6, modNwSrc $ IPAddress 50)]

pr1 = inport 1 0
pr2 = inport 1 0 <||> inport 2 3
pr3 = inport 3 3 <&&> DlSrc (EthernetAddress 10)
pr4 = pr3 <&&> Not (DlDst (EthernetAddress 20))

po1 = pr1 ==> a1
po2 = pr2 ==> a2
po3 = pr3 ==> a3
po4 = pr4 ==> a4
po5 = pr1 <&&> pr4 ==> a5

bigPolicy = ((po3 <+> po4 <+> po5) <%> pr2) <+> po1 <+> po2
baseForwards = forwardsOfPolicy bigPolicy

forwardsOfPolicy PoBottom        = []
forwardsOfPolicy (PoBasic _ acts) = filter isForward acts
forwardsOfPolicy (PoUnion p1 p2) = forwardsOfPolicy p1 ++
                                   forwardsOfPolicy p2
forwardsOfPolicy (Restrict pol _) = forwardsOfPolicy pol

setVl v (Forward p m) = Forward p (m { modifyDlVlan = Just v })
setVl v a = a

case_testModifyVlan = do
  let expected = map (setVl (Just 1234)) baseForwards
  let observedPolicy = modifyVlan (Just 1234) bigPolicy
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
  let expected = 
        [Forward (Physical 2) 
                              (unmodified { modifyNwDst = Just (IPAddress 30)
                                          , modifyDlVlan = Just (Just 1234)})]
  let observed = forwardsOfPolicy $ setVlan (Just 1234) (Loc 1 2) pol
  assertEqual "setVlan set vlan on Loc 1 2" expected observed

case_testSetVlanComplex = do
  let pol = (pr3 ==> a1) <+> (pr3 ==> a5)
  let setVl (Forward p m) 
        | p == Physical 3 = Forward p (m { modifyDlVlan = Just (Just 1234) })
        | otherwise = Forward p m
      setVl act = act
  let expected = map setVl (forwardsOfPolicy pol)
  let observed = forwardsOfPolicy $ setVlan (Just 1234) (Loc 3 3) pol
  assertEqual "setVlan set vlan on Loc 1 3 across union" expected observed
