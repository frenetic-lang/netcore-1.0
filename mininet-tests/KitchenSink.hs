module KitchenSink (sink, main) where

import Arp (doArp)
import MacLearning (learningSwitch)
import Monitor (monitor)

import Control.Concurrent
import Data.ByteString.Lazy (ByteString)
import qualified Data.Set as Set

import Frenetic.NetCore
import Frenetic.Slices.Compile (dynamicTransform)
import Frenetic.Slices.Slice (simpleSlice)
import Frenetic.Topo (buildGraph)

-- This is the default topology when you start mininet without parameters
topo = buildGraph [ ((2, 0), (1, 1))
                  , ((3, 0), (1, 2)) ]

spy = do
  spyChan <- newChan
  (byteChan, action) <- countBytes 10000 -- every 10 seconds
  writeChan spyChan (matchAll ==> action)
  return spyChan

-- |Run a number of examples wrapped in slices.
-- * Arp in a slice that only accepts ARP packets
-- * Learning Switch in a slice that only accepts ipv4 packets
-- Note that this is built to only work on the default mininet topology of
-- H2 --1 S1 2-- H3
sink :: IO (Chan Policy) -> IO (Chan Policy, Chan (Loc, ByteString))
sink routing = do
  route <- routing
  arpRoute <- routing
  monitorRoute <- routing
  spyPolicy <- spy
  -- Just ARP
  let arpSlice = simpleSlice topo (dlTyp 0x0806)
  -- Just ICMP (ping)
  let monitorSlice = simpleSlice topo (dlTyp 0x0800 <&&> nwProto 0x01)
  -- Everything else
  let routeSlice = simpleSlice topo (neg $ (dlTyp 0x0806 <||>
                                           (dlTyp 0x0800 <&&> nwProto 0x01)))
  let spySlice = simpleSlice topo matchAll
  (arpPolicy, arpPacket) <- doArp arpRoute
  monitorPolicy <- monitor monitorRoute
  policies <- dynamicTransform [ (routeSlice, route)
                               , (arpSlice, arpPolicy)
                               , (monitorSlice, monitorPolicy)
                               , (spySlice, spyPolicy)
                               ]
  return (policies, arpPacket)

main = do
  (policies, packets) <- sink learningSwitch
  dynController policies packets
