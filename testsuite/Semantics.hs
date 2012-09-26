module Semantics where

import Common
import Frenetic.NetCore.Semantics
import Frenetic.NetCore.Types

tests = $(testGroupGenerator)

prop_simpleFwd = do
  inp <- arbInPkt
  case evalPol (PolProcessIn (Switch 3) [ActFwd AllPorts unmodified]) inp of
    [OutPkt 3 AllPorts _ _] -> label "on 3" True
    [] -> label "trivially true" True
    out -> printTestCase (printf "inp=%s\nout=%s" (show inp) (show out)) False

prop_dropAll = do
  inp <- arbitrary
  let out = evalPol (PolProcessIn None [ActFwd AllPorts unmodified]) inp
  label "should drop" (null out)