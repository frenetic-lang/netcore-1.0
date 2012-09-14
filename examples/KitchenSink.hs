module KitchenSink (sink, main) where

import Arp (doArp)
import MacLearning (learningSwitch)
import Monitor (monitor)

import Control.Concurrent
import Data.ByteString.Lazy (ByteString)
import qualified Data.Set as Set

import Frenetic.NetCore

-- This is the default topology when you start mininet without parameters
topo = buildGraph [ ((2, 0), (1, 1))
                  , ((3, 0), (1, 2)) ]

spy = do
  spyChan <- newChan
  (byteChan, action) <- countBytes 10000 -- every 10 seconds
  writeChan spyChan (Any ==> action)
  return spyChan

-- |Run a number of examples wrapped in slices.
-- * Arp in a slice that only accepts ARP packets
-- * Learning Switch in a slice that only accepts ipv4 packets
-- Note that this is built to only work on the default mininet topology of
-- H2 --1 S1 2-- H3
sink :: IO (Chan Policy) -> IO (Chan Policy)
sink routing = do
  route <- routing
  arpRoute <- routing
  monitorRoute <- routing
  spyPolicy <- spy
  -- Just ARP
  let arpSlice = simpleSlice topo (DlTyp 0x0806)
  -- Just ICMP (ping)
  let monitorSlice = simpleSlice topo (DlTyp 0x0800 <&&> NwProto 0x01)
  -- Everything else
  let routeSlice = simpleSlice topo (Not (prOr [ DlTyp 0x0806
                                               , DlTyp 0x0800 <&&> NwProto 0x01]))
  let spySlice = simpleSlice topo Any
  arpPolicy <- doArp arpRoute
  monitorPolicy <- monitor monitorRoute
  policies <- dynTransform [ (routeSlice, route)
                               , (arpSlice, arpPolicy)
                               , (monitorSlice, monitorPolicy)
                               , (spySlice, spyPolicy)
                               ]
  return policies

main = do
  policies <- sink learningSwitch
  dynController policies
