module Tests.Frenetic.Slices.TestVerification where

import Frenetic.NetCore.Semantics
import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.Framework.Providers.HUnit

import Frenetic.NetCore
import Frenetic.NetCore.Short
import Frenetic.Slices.Slice
import Frenetic.Slices.Verification

import qualified Data.Set as Set
import qualified Data.Map as Map

sliceVerificationTests = $(testGroupGenerator)

s1 = Slice (Set.fromList [ Loc 1 0
                         , Loc 1 1
                         , Loc 2 0
                         , Loc 3 0
                         , Loc 4 0
                         , Loc 5 0 ])
           (Map.singleton (Loc 6 0) top)
           (Map.singleton (Loc 7 0) top)
s2 = Slice (Set.fromList [ Loc 10 0
                         , Loc 10 1
                         , Loc 20 0
                         , Loc 30 0
                         , Loc 40 0
                         , Loc 50 0 ])
           (Map.singleton (Loc 60 0) top)
           (Map.singleton (Loc 70 0) top)
s3 = Slice (Set.singleton (Loc 1 0)) Map.empty Map.empty
s4 = Slice Set.empty (Map.singleton (Loc 6 0) top) Map.empty
s5 = Slice Set.empty Map.empty (Map.singleton (Loc 7 0) top)

case_testSeparated = do
  False @=? separated s1 s1
  True @=? separated s1 s2
  False @=? separated s1 s3
  False @=? separated s1 s4
  False @=? separated s1 s5

case_testDisjoint = do
  False @=? edgeDisjoint s1 s1
  True @=? edgeDisjoint s1 s2
  False @=? edgeDisjoint s1 s3
  True @=? edgeDisjoint s1 s4
  True @=? edgeDisjoint s1 s5
