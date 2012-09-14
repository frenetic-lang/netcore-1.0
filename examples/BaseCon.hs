module BaseCon where

import Control.Concurrent
import Control.Monad
import Frenetic.NetCore
import System.IO


main = do
  (c1, query1Act) <- countPkts 1000
  (c2, query2Act) <- countPkts 1000
  let monitoringOne = (DlDst (ethernetAddress 0 0 0 0 0 2) ==> query1Act)
  let monitoringTwo = (DlDst (ethernetAddress 0 0 0 0 0 3) ==> query2Act)
  let monitoringPolicy = PoUnion monitoringOne monitoringTwo
  let forwarding1 = (DlDst (ethernetAddress 0 0 0 0 0 2) ==> forward [1]) 
  let forwarding2 = (DlDst (ethernetAddress 0 0 0 0 0 3) ==> forward [2])
  let forwardingPolicy = PoUnion forwarding1 forwarding2
  --let pol = (Any ==> allPorts unmodified) `PoUnion` (Any ==> queryAct)
  forkIO $ forever $ do
    (sw, n) <- readChan c1
    putStrLn ("Counter for h2 is: " ++ show n)
    (sw, n) <- readChan c2
    putStrLn ("Counter for h3 is: " ++ show n)
    hFlush stdout
  controller (PoUnion forwardingPolicy monitoringPolicy)




--module BaseCon where

--import Frenetic.NetCore

--topo = buildGraph [ ((2, 0), (1, 1))
--                  , ((3, 0), (1, 2)) ]


--policy = Any ==> allPorts unmodified      --h2 to h3 
              ----h3 to h2
              --monitor

--main = controller policy
