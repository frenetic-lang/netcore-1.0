module BaseMon where

import Control.Concurrent
import Control.Monad
import Frenetic.NetCore
import System.IO
import Frenetic.Topo
import Repeater
import Data.Word

--      H1
--       |
--      S
--     /  \
--   H2    H3

s = 101
h1 = 1
h2 = 2
h3 = 3

ns = [s,h1,h2,h3]
el= [((Frenetic.Topo.Switch 101, 1),(Host 1,0)), ((Frenetic.Topo.Switch 101, 2), (Host 2,0)), ((Frenetic.Topo.Switch 101,3), (Host 3,0))]

topo :: Graph
topo = buildGraph el

mkMonitorPolicy :: Policy -> Graph -> IO ()
mkMonitorPolicy fwd g = 
  let monitorCallback ei ( sw, n ) = do
        putStrLn ("Counter for " ++ show ei ++ " on " ++ show sw ++ " is: " ++ show n) in 
  let p = foldl (\acc h -> (DlDst (ethernetAddress 0 0 0 0 0 (fromIntegral h :: Word8))
        ==> [GetPacket 0 (monitorCallback h)]) `PoUnion` acc) PoBottom (hosts g)  in
     controller (fwd `PoUnion` p)

--myMain :: IO ()
--myMain = mkMonitorPolicy (OneRes.policy) topo

main = do
  mkMonitorPolicy (Repeater.policy) topo

--main = do
--  (c1, query1Act) <- countPkts 1000
--  (c2, query2Act) <- countPkts 1000
--  let monitoringOne = (DlDst (ethernetAddress 0 0 0 0 0 2) <&&> TpSrcPort 80 ==> query1Act)
--  let monitoringTwo = (DlDst (ethernetAddress 0 0 0 0 0 3) <&&> TpSrcPort 80 ==> query2Act)
--  let monitoringPolicy = PoUnion monitoringOne monitoringTwo
--  let forwarding1 = (DlDst (ethernetAddress 0 0 0 0 0 2)  ==> forward [1]) 
--  let forwarding2 = (DlDst (ethernetAddress 0 0 0 0 0 3)  ==> forward [2])
--  let forwardingPolicy = PoUnion forwarding1 forwarding2
--  --let pol = (Any ==> allPorts unmodified) `PoUnion` (Any ==> queryAct)
--  forkIO $ forever $ do
--    (sw, n) <- readChan c1
--    putStrLn ("Web packets to h2: " ++ show n)
--    (sw, n) <- readChan c2
--    putStrLn ("Web packets to h3: " ++ show n)
--    hFlush stdout
--  controller (PoUnion forwardingPolicy monitoringPolicy)
--
