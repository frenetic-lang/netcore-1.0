module Semantics where

import qualified Data.Map as M
import Common
import Frenetic.NetCore.Semantics
import Frenetic.NetCore.Types
import Frenetic.NetCore.Reduce (isEmptyPredicate)

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

simplCallbackDelayStream cbs = map f (callbackDelayStream cbs)
  where f v = case v of
          Left (id, _) -> Left id
          Right t -> Right t

case_callbackDelayStream1 = do
  let cbs = M.fromList [ (50, CallbackPktCounter 3 (\_ -> return ()))
                       ]
  let stream = take 4 (simplCallbackDelayStream cbs)
  let expected = [Right 3, Left 50, Right 3, Left 50]
  assertEqual "should be equal"  expected stream


case_callbackDelayStream2 = do
  let cbs = M.fromList [ (50, CallbackPktCounter 3 (\_ -> return ()))
                       , (75, CallbackPktCounter 5 (\_ -> return ()))
                       ]
  let expected = [ Right 3, Left 50
                 , Right 2, Left 75 
                 , Right 1, Left 50
                 , Right 3, Left 50
                 , Right 1, Left 75 
                 , Right 2, Left 50
                 , Right 3, Left 50, Left 75 -- 15 timesteps, both fire
                 ]
  let stream = take (length expected) (simplCallbackDelayStream cbs)
  assertEqual "should be equal" expected stream