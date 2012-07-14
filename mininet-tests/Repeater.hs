module Repeater where

import Frenetic.NetCore
import Control.Concurrent.Chan

policy = PoBasic (PrPattern top) flood

main = do
  polChan <- newChan
  writeChan polChan policy
  freneticServer polChan
