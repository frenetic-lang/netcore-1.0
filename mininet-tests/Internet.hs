module Internet where

import Frenetic.NetCore
import Control.Concurrent.Chan

-- IP packets with destination not 10.0.0/24
public = -- PrIntersect (PrPattern $ top { ptrnDlTyp = Exact 0x800 })
                 (neg $ nwDstPrefix 0x0a000000 24)

policy = (neg public ==> allPorts unmodified) <+>
         (public ==> forward [3])

main = do
  polChan <- newChan
  writeChan polChan policy
  freneticServer polChan
