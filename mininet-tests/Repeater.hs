module Repeater where

import Frenetic.NetCore
import Control.Concurrent.Chan

policy = matchAll ==> allPorts unmodified

main = do
  polChan <- newChan
  writeChan polChan policy
  freneticServer polChan
