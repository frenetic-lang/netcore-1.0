module BaseCon where

import Control.Concurrent
import Control.Monad
import Frenetic.NetCore
import System.IO
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree
import Repeater

--      H1
--       |
--      S
--     /  \
--   H2    H3

data N = S Switch
       | H EthernetAddress

s = (0,S 101)
h1 = (1,H (ethernetAddress 0 0 0 0 0 1))
h2 = (2,H (ethernetAddress 0 0 0 0 0 2))
h3 = (3,H (ethernetAddress 0 0 0 0 0 3))

ns = [s,h1,h2,h3]

es = [(0,1,()), (0,2,()), (0,3,())]

topo :: Gr N ()
topo = mkGraph ns es

type Topology = Gr N ()

mkMonitorPolicy :: Policy -> Topology -> IO ()
mkMonitorPolicy fwd g = 
  let hosts = foldl (\acc (_,t) -> case t of {H e -> e:acc; _ -> acc }) [] (labNodes g) in 
  let monitorCallback ei ( sw, n ) = do
        putStrLn ("Counter for " ++ show ei ++ " on " ++ show sw ++ " is: " ++ show n) in 
  let p = foldl (\acc e -> (DlDst e ==> [GetPacket 0 (monitorCallback e)]) `PoUnion` acc) PoBottom hosts  in
     controller (fwd `PoUnion` p)

myMain :: IO ()
myMain = mkMonitorPolicy (Repeater.policy) topo

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

