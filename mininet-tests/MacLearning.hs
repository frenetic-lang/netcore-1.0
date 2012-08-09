-- ^Uses NetCore to implement mac learning
--
-- Does not work on networks with loops.
module MacLearning where

import Control.Concurrent
import Frenetic.NetCore
import qualified Data.Map as Map
import Frenetic.NetCore.Types (poDom)

isFlood = dlDst broadcastAddress

-- |Learns the location of hosts at each switch.
--
-- The NetCore program inspects packets at all switches to learn the route
-- to each host from each switch. The route to a host is the port on which
-- the switch receives packets from that host. The program handles
-- host mobility: if the host starts sending from a new port, the program
-- learns the new location of the host. 
pktsByLocation :: IO (Chan (Word48, Loc), Chan Policy)
pktsByLocation = do
  uniqPktChan <- newChan
  polChan <- newChan
  (pktChan, act) <- getPkts
  let loop :: Map.Map (Switch, Word48) (Port, Predicate) -> IO ()
      loop locs = do
        (sw, pkt) <- readChan pktChan
        let port = pktInPort pkt
        let srcMac = pktDlSrc pkt
        case Map.lookup (sw, srcMac) locs of
          Just (port', _) | port == port' -> do
            -- Not a new location, nothing to do.
            loop locs
          otherwise -> do
            -- pktDlSrc pkt is either completely new or has moved.
            let pred = onSwitch sw <&&> dlSrc srcMac <&&> inPort port
            let locs' = Map.insert (sw, srcMac) (port, pred) locs
            writeChan uniqPktChan (srcMac, Loc sw port)
            writeChan polChan $
              neg (isFlood <||> prOr (map snd (Map.elems locs'))) ==> act
            loop locs'
  -- Initially, inspect all packets except floods.
  writeChan polChan (neg isFlood ==> act)
  forkIO (loop Map.empty)
  return (uniqPktChan, polChan)

learnRoutes :: Chan (Word48, Loc) -> IO (Chan Policy)
learnRoutes pktChan = do
  polChan <- newChan
  let mkRule locs ((sw, dstMac), port) = 
        map (\((_, srcMac), _) -> 
          (onSwitch sw <&&> dlSrc srcMac <&&> dlDst dstMac) ==> forward [port]) $
          filter (\((sw', _), _) -> sw' == sw) $
            Map.toList locs
  let loop :: Map.Map (Switch, Word48) Port -> IO ()
      loop locs = do
        (dlSrc, Loc sw pt) <- readChan pktChan
        let locs' = Map.insert (sw, dlSrc) pt locs
        let fwdPol = foldr (<+>) PoBottom (concatMap (mkRule locs') (Map.toList locs'))
        let floodPol = neg (poDom fwdPol) ==> allPorts unmodified
        writeChan polChan (fwdPol <+> floodPol)
        loop locs'
  writeChan polChan (matchAll ==> allPorts unmodified)
  forkIO (loop Map.empty)
  return polChan

learningSwitch = do
  (uniqPktsChan, queryPolChan) <- pktsByLocation
  fwdPolChan <- learnRoutes uniqPktsChan
  bothPolsChan <- select queryPolChan fwdPolChan
  polChan <- newChan
  let loop fwdPol queryPol = do
        pol <- readChan bothPolsChan
        case (pol, fwdPol, queryPol) of
          (Left queryPol, Nothing, _) -> loop Nothing (Just queryPol)
          (Right fwdPol, _, Nothing) -> loop (Just fwdPol) Nothing
          (Left queryPol, Just fwdPol, _) -> do
            writeChan polChan (fwdPol <+> queryPol)
            loop (Just fwdPol) (Just queryPol)
          (Right fwdPol, _, Just queryPol) -> do
            writeChan polChan (fwdPol <+> queryPol)
            loop (Just fwdPol) (Just queryPol)
  forkIO (loop Nothing Nothing)
  return polChan

main = do
  polChan <- learningSwitch
  pktChan <- newChan
  dynController polChan pktChan
