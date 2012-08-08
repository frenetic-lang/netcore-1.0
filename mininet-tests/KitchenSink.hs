module KitchenSink (sink, main) where

import Arp (doArp)
import MacLearning (learningSwitch)

import Control.Concurrent
import Data.ByteString.Lazy (ByteString)
import qualified Data.Set as Set

import Frenetic.NetCore
import Frenetic.Slices.Compile (dynamicTransform)
import Frenetic.Slices.Slice (simpleSlice)
import Frenetic.Topo (buildGraph)

topo = buildGraph [ ((2, 0), (1, 1))
                  , ((3, 0), (1, 2)) ]

-- |Run a number of examples wrapped in slices.
-- * Arp in a slice that only accepts ARP packets
-- * Learning Switch in a slice that only accepts ipv4 packets
-- Note that this is built to only work on the default mininet topology of
-- H2 --1 S1 2-- H3
sink :: IO (Chan Policy) -> IO (Chan Policy, Chan (Loc, ByteString))
sink routing = do
  route <- routing
  arpRoute <- routing
  let routeSlice = simpleSlice topo (dlTyp 0x0800)
  let arpSlice = simpleSlice topo (dlTyp 0x0804)
  (arpPolicy, arpPacket) <- doArp arpRoute
  policies <- dynamicTransform [ (routeSlice, route)
                               , (arpSlice, arpPolicy)
                               ]
  return (policies, arpPacket)

main = do
  (policies, packets) <- sink learningSwitch
  dynController policies packets
