module Semantics where

import Test.QuickCheck
import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Arbitrary
import Frenetic.NetCore.Semantics
import Frenetic.NetCore.Types

tests = $(testGroupGenerator)

case_simpleFwd = quickCheck $ do
  inp <- arbInPkt
  case evalPol (PolProcessIn (Switch 3) [ActFwd AllPorts unmodified]) inp of
    [OutPkt 3 AllPorts _ _] -> return True
    [] -> return True
    otherwise -> return False

prop_dropAll :: In -> Bool
prop_dropAll inp = 
  evalPol (PolProcessIn None [ActFwd AllPorts unmodified]) inp == []