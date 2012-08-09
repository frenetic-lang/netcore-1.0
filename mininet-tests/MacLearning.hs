-- ^Uses NetCore to implement a learning switch.
--
-- Does not work on networks with loops.
module MacLearning where

import Control.Concurrent
import Control.Monad (forever)
import Frenetic.NetCore
import qualified Data.Map as Map
import Frenetic.NetCore.Types (poDom)

isFlood = dlDst broadcastAddress

-- |Learns the location of hosts at each switch.
--
-- 'pktsByLocation' produces a stream of NetCore policies and a stream of
-- hosts' locations. The NetCore policies inspect packets at all switches.
-- At each switch, the NetCore policies learn the ingress port that receives
-- packets from each host. These learned locations are output to the other
-- stream.
--
--  The program handles host mobility: if the host starts sending from
--  a new port, the program learns the new location of the host.
pktsByLocation :: IO (Chan (EthernetAddress, Loc), Chan Policy)
pktsByLocation = do
  locChan <- newChan -- hosts' locations
  polChan <- newChan -- policy to monitor new hosts and moved hosts
  (pktChan, act) <- getPkts
  -- In a loop, inspect packets, and learn the ingress port for hosts
  let loop :: Map.Map (Switch, EthernetAddress) (Port, Predicate) 
           -> IO ()
      loop locs = do
        (sw, pkt) <- readChan pktChan
        let port = pktInPort pkt
        let srcMac = pktDlSrc pkt
        case Map.lookup (sw, srcMac) locs of
          Just (port', _) | port == port' -> do
            -- We aleady know the host and its location is unchanged.
            loop locs
          otherwise -> do
            -- Either (1) srcMac has never sent a packet to the switch, or
            -- (2) it has before, but from a different port.
            let pred = dlSrc srcMac <&&>
                       onSwitch sw <&&>
                       inPort port
            let locs' = Map.insert (sw, srcMac) (port, pred) locs
            writeChan locChan (srcMac, Loc sw port)
            -- Update the policy so that we do not see packets from known
            -- hosts at known locations.
            let knownHosts = prOr (map snd (Map.elems locs'))
            writeChan polChan $
              neg (dlSrc broadcastAddress <||> knownHosts) ==> act
            loop locs'
  -- Initially, inspect all packets (excluding broadcasts)
  writeChan polChan (neg (dlSrc broadcastAddress) ==> act)
  forkIO (loop Map.empty)
  return (locChan, polChan)

-- |'fwdTo locs ((sw, dstMac), port)' builds a policy that forwards
-- packets to 'dstMac', at switch 'sw' on port 'port' from hosts in
-- 'locs'. The policy only forwards packets on 'sw', so it ignores
-- members of 'locs' on other switches.
fwdTo :: Map.Map (Switch, EthernetAddress) Port
      -> ((Switch, EthernetAddress), Port) -- ^forwards to this
      -> Policy
fwdTo locs ((sw, dstMac), port) = foldr (<+>) PoBottom (map f srcMacs)
  where -- select only srcMacs at sw
        srcMacs = map (\((_, srcMac), _) -> srcMac) $
                    filter (\((sw', _), _) -> sw' == sw)
                      (Map.toList locs)
        f srcMac = (onSwitch sw <&&> dlSrc srcMac <&&> dlDst dstMac)
                   ==> forward [port]

-- |'placeRoutes pktChan' produces a stream of NetCore forwarding policies that
-- forward packets based on hosts' locations in 'pktChan'.
placeRoutes :: Chan (EthernetAddress, Loc) -> IO (Chan Policy)
placeRoutes pktChan = do
  polChan <- newChan
  -- When a hosts location arrives, update the map and recompute the forwarding
  -- policy.
  let loop :: Map.Map (Switch, EthernetAddress) Port -> IO ()
      loop locs = do
        (dlSrc, Loc sw pt) <- readChan pktChan
        let locs' = Map.insert (sw, dlSrc) pt locs
        let fwdPol = foldr (<+>) PoBottom (map (fwdTo locs') (Map.toList locs'))
        -- Flood all other packets to maintain connectivity.
        let floodPol = neg (poDom fwdPol) ==> allPorts unmodified
        writeChan polChan (fwdPol <+> floodPol)
        loop locs'
  -- Initially, flood all packets
  writeChan polChan (matchAll ==> allPorts unmodified)
  forkIO (loop Map.empty)
  return polChan

learningSwitch = do
  (uniqPktsChan, queryPolChan) <- pktsByLocation
  fwdPolChan <- placeRoutes uniqPktsChan
  bothPolsChan <- both queryPolChan fwdPolChan
  polChan <- newChan
  forkIO $ forever $ do
    (queryPol, fwdPol) <- readChan bothPolsChan
    writeChan polChan (fwdPol <+> queryPol)
  return polChan

main = do
  polChan <- learningSwitch
  pktChan <- newChan
  dynController polChan pktChan
