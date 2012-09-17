module BaseCon where

import Control.Concurrent
import Control.Monad
import Frenetic.NetCore
import System.IO


main = do
  (c1, query1Act) <- countPkts 1000
  (c2, query2Act) <- countPkts 1000
  let monitoringOne = (DlDst (ethernetAddress 0 0 0 0 0 2) <&&> TpSrcPort 80 ==> query1Act)
  let monitoringTwo = (DlDst (ethernetAddress 0 0 0 0 0 3) <&&> TpSrcPort 80 ==> query2Act)
  let monitoringPolicy = PoUnion monitoringOne monitoringTwo
  let forwarding1 = (DlDst (ethernetAddress 0 0 0 0 0 2)  ==> forward [1]) 
  let forwarding2 = (DlDst (ethernetAddress 0 0 0 0 0 3)  ==> forward [2])
  let forwardingPolicy = PoUnion forwarding1 forwarding2
  --let pol = (Any ==> allPorts unmodified) `PoUnion` (Any ==> queryAct)
  forkIO $ forever $ do
    (sw, n) <- readChan c1
    putStrLn ("Web packets to h2: " ++ show n)
    (sw, n) <- readChan c2
    putStrLn ("Web packets to h3: " ++ show n)
    hFlush stdout
  controller (PoUnion forwardingPolicy monitoringPolicy)

