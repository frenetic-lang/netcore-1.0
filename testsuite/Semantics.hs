module Semantics where

import Text.Printf
import Test.QuickCheck
import Test.HUnit hiding (Testable)
import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Arbitrary
import Frenetic.NetCore.Semantics
import Frenetic.NetCore.Types

tests = $(testGroupGenerator)

assertQuickCheck :: Property -> Assertion
assertQuickCheck body = do
  result <- quickCheckResult body
  case result of
    Success _ _ _ -> return ()
    otherwise -> assertFailure ""

case_simpleFwd = assertQuickCheck $ do
  inp <- arbInPkt
  case evalPol (PolProcessIn (Switch 3) [ActFwd AllPorts unmodified]) inp of
    [OutPkt 3 AllPorts _ _] -> label "on 3" True
    [] -> label "trivially true" True
    out -> printTestCase (printf "inp=%s\nout=%s" (show inp) (show out)) False

prop_dropAll :: In -> Bool
prop_dropAll inp = 
  evalPol (PolProcessIn None [ActFwd AllPorts unmodified]) inp == []