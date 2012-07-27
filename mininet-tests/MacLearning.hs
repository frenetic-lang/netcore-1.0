module MacLearning where

import Control.Concurrent
import Control.Monad
import Frenetic.NetCore
import Frenetic.Util
import System.IO
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.MultiSet as MS
import System.Log.Logger

-- TODO(arjun): Doesn't work with forwarding loops.
pktsByLocation :: IO (Chan (Switch, Packet), Chan Policy)
pktsByLocation = do
  uniqPktChan <- newChan
  polChan <- newChan
  (pktChan, q) <- pktQuery 
  let act = Action MS.empty [q]
  let matchLoc ((sw, srcMac), port) = PrTo sw <&> dlSrc srcMac <&> inPort port
  let loop locs = do
        (sw, pkt) <- readChan pktChan
        case Map.lookup (sw, pktDlSrc pkt) locs of
          Just port | pktInPort pkt == port -> do
            -- Not a new location, nothing to do.
            loop locs
          otherwise -> do
            -- pktDlSrc pkt is either completely new or has moved.
            let locs' = Map.insert (sw, pktDlSrc pkt) (pktInPort pkt) locs
            let pred = PrNegate (prNaryUnion (map matchLoc (Map.toList locs')))
            writeChan uniqPktChan (sw, pkt)
            writeChan polChan (PoBasic pred act)
            loop locs'
  writeChan polChan (PoBasic top act)
  forkIO (loop Map.empty)
  return (uniqPktChan, polChan)

learnRoutes :: Chan (Switch, Packet) -> IO (Chan Policy)
learnRoutes pktChan = do
  polChan <- newChan
  let mkRule locs ((sw, dstMac), port) = 
        map (\((_, srcMac), _) -> 
          PoBasic (PrTo sw <&> dlSrc srcMac <&> dlDst dstMac) (forward port)) $
          filter (\((sw', _), _) -> sw' == sw) $
            Map.toList locs
  let loop :: Map (Switch, Word48) Port -> IO ()
      loop locs = do
        (sw, pkt) <- readChan pktChan
        let locs' = Map.insert (sw, pktDlSrc pkt) (pktInPort pkt) locs
        let fwdPol = poNaryUnion (concatMap (mkRule locs') (Map.toList locs'))
        debugM "maclearning" $ "forwarding policy is " ++ show fwdPol
        let floodPol = PoBasic (PrNegate (poDom fwdPol)) flood
        writeChan polChan (PoUnion fwdPol floodPol)
        loop locs'
  writeChan polChan (PoBasic top flood)
  forkIO (loop Map.empty)
  return polChan

learningSwitch = do
  (uniqPktsChan, queryPolChan) <- pktsByLocation
  fwdPolChan <- learnRoutes uniqPktsChan
  bothPolsChan <- mergeChan queryPolChan fwdPolChan
  polChan <- newChan
  let loop fwdPol queryPol = do
        pol <- readChan bothPolsChan
        case (pol, fwdPol, queryPol) of
          (Left queryPol, Nothing, _) -> loop Nothing (Just queryPol)
          (Right fwdPol, _, Nothing) -> loop (Just fwdPol) Nothing
          (Left queryPol, Just fwdPol, _) -> do
            writeChan polChan (PoUnion fwdPol queryPol)
            loop (Just fwdPol) (Just queryPol)
          (Right fwdPol, _, Just queryPol) -> do
            writeChan polChan (PoUnion fwdPol queryPol)
            loop (Just fwdPol) (Just queryPol)
  forkIO (loop Nothing Nothing)
  return polChan

main = do
  polChan <- learningSwitch
  freneticServer polChan
