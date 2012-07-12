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

import Data.MultiSet as MS

sliceCompileTests = $(testGroupGenerator)

-- Construct a bunch of basically meaningless objects for testing

a1 = Action (MS.fromList [ (Physical 1, dlSrc 10)
                         , (Physical 2, dlSrc 10)
                         , (Physical 3, dlSrc 10)])
            []
a2 = Action (MS.fromList [ (Physical 2, dlDst 20)
                         , (Physical 3, dlDst 20)])
            []
a3 = Action (MS.fromList [ (Physical 2, dlTyp 30)])
            []
a4 = Action (MS.fromList [ (Physical 4, nwSrc 40)
                         , (Physical 5, nwSrc 40)
                         , (Physical 6, nwSrc 40)])
            []
a5 = Action (MS.fromList [ (Physical 3, nwSrc 50)
                         , (Physical 4, nwSrc 50)
                         , (Physical 5, nwSrc 50)
                         , (Physical 6, nwSrc 50)])
            []

pr1 = inport 1 0
pr2 = inport 1 0 <|> inport 2 3
pr3 = inport 3 3 <&> PrPattern (dlSrc 10)
pr4 = pr3 <&> neg (PrPattern (dlDst  20))

po1 = pr1 ==> a1
po2 = pr2 ==> a2
po3 = pr3 ==> a3
po4 = pr4 ==> a4
po5 = pr1 <&> pr4 ==> a5

bigPolicy = ((po3 <+> po4 <+> po5) % pr2) <+> po1 <+> po2

forwardsOfPolicy PoBottom = MS.empty
forwardsOfPolicy (PoUnion p1 p2) = MS.union (forwardsOfPolicy p1)
                                          (forwardsOfPolicy p2)
forwardsOfPolicy (PoBasic _ a) = forwardsOfAction a

forwardsOfAction (Action ms _) = ms

case_testModifyVlan = do
  assertEqual "modifyVlan puts vlan tags on all forwards"
    expected observedForwards
  where
    baseForwards = forwardsOfPolicy bigPolicy
    expected = MS.map (\ (port, pat) -> (port, pat {ptrnDlVlan = exact 1234}))
                         baseForwards
    observedPolicy = modifyVlan 1234 bigPolicy
    observedForwards = forwardsOfPolicy observedPolicy
