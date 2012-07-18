
module Tests.Frenetic.Slices.TestSlice where

import Frenetic.NetCore.Semantics
import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.Framework.Providers.HUnit

import Frenetic.NetCore
import Frenetic.NetCore.Short
import Frenetic.NetCore.Semantics

import Frenetic.Slices.Slice

import qualified Data.Map as Map
import qualified Data.MultiSet as MS
import qualified Data.Set as Set

sliceTests = $(testGroupGenerator)

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

pr1 = inport 1 0
pr2 = inport 1 0 <|> inport 2 3
pr3 = inport 3 3 <&> dlSrc 10
pr4 = pr3 <&> neg (dlDst  20)

po1 = pr1 ==> a1
po2 = pr2 ==> a2
po3 = pr3 ==> a3
po4 = pr4 ==> a4

case_testSwitchesOfPredicate = do
  let switches = Set.fromList [1 .. 10]
  let sop = switchesOfPredicate
  Set.fromList [1] @=? sop switches pr1
  Set.fromList [1, 2] @=? sop switches pr2
  Set.fromList [3] @=? sop switches pr3
  Set.fromList [3] @=? sop switches pr4
  Set.fromList [] @=? sop switches (inport 1 0 <&> inport 2 0)

case_testPoUsesVlans = do
  False @=? poUsesVlans (po1 <+> po2 <+> po3 <+> po4)
  True @=? poUsesVlans (PrPattern (patDlVlan 3) ==> Action MS.empty [])
  True @=? poUsesVlans (top ==> Action (MS.singleton (Physical 1, patDlVlan 3)) [])

slice = Slice (Set.fromList [ Loc 1 1
                            , Loc 1 2
                            , Loc 2 3
                            , Loc 3 2
                            ])
              (Map.singleton (Loc 1 0) top)
              (Map.singleton (Loc 3 3) top)

--TODO(astory): Finish implementing when we understand what a Transmission is.
--case_testLocalize = do
--  let policy = po1 <+> po2 <+> po3 <+> po4
--  let localized = localize slice (policy)
--  assertEqual "Packet incident to Loc 1 0"
--              (Action (MS.fromList [ (Physical 1, dlSrc 10)
--                                   , (Physical 2, dlSrc 10)
--                                   , (Physical 2, dlDst 20)])
--                      [])
--              (interpretPolicy policy undefined)
