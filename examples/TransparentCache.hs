module TransparentCache where

import Control.Concurrent
import Control.Concurrent.MSampleVar
import Control.Monad
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.ByteString.Lazy (ByteString)
import Frenetic.NetCore
import Frenetic.NetworkFrames (arpReply)
import System.Log.Logger
import System.IO

-- TODO: 
--   MAC rewriting
--   simplify!


-- Heartbeat --
-- For now, ping request packets count as the heartbeat.
isIP = DlTyp 0x800
isArp = DlTyp 0x806
heartbeatPred = isIP <&&> NwProto 1 <&&> NwTos 0
heartbeatDelay = 7 * 1000 * 1000 -- microseconds

-- Monitors for heartbeat packets from a given set of IP addresses and
-- periodically emits a set of ports that are still alive (i.e. are connected
-- to a TC that has sent a heartbeat in the most recent time interval).
monitorHeartbeats :: Set.Set IPAddress -> IO (MSampleVar (Set.Set Port), Policy)
monitorHeartbeats targetIPs = do
  svar <- newEmptySV
  (pktChan, act) <- getPkts
  let pol = heartbeatPred ==> act
  -- In a loop, delay for heartbeatDelay milliseconds, then read all the
  -- heartbeat messages in the channel.  Any host that sent a message is
  -- still alive.
  let loop ports = do
      -- Unreachable for testing ...
      isEmpty <- isEmptyChan pktChan
      case isEmpty of
        False -> do
            ((Loc switch port), heartbeatPkt) <- readChan pktChan
            infoM "heartbeat" $ "packet in: " ++ show heartbeatPkt
            case pktNwSrc heartbeatPkt of
              Nothing -> loop ports
              Just ip
                | Set.member ip targetIPs -> loop $ Set.insert port ports
                | otherwise -> loop ports
        True -> do
            infoM "heartbeat" $ "writing live ports: " ++ show ports
            writeSV svar ports
            threadDelay heartbeatDelay
            loop Set.empty
  let threadStart = do
      threadDelay heartbeatDelay
      loop Set.empty
  forkIO threadStart
  return (svar, pol)


-- Load Balancer --
-- TODO: currently drops the first packet of each new flow.

-- Generate a stream of policies forwarding IP flows out
-- ports drawn from the set of live ports pulled from the
-- livePortsSV channel.
balance :: MSampleVar (Set.Set Port) -> IO (Chan Policy)
balance livePortsSV = do
  (ipChan, ipAct) <- getPkts
  polChan <- newChan
  let loop :: Set.Set Port -> [Port] -> Map.Map IPAddress Port -> IO ()
      -- If there are no live ports, wait until some show up.
      loop livePorts _ ip2port | Set.null livePorts = do
          -- TODO: deploy a temporary policy forwarding everything?
          infoM "balance" "no live ports."
          livePorts' <- readSV livePortsSV
          loop livePorts' (Set.toList livePorts') ip2port

      -- If the portList is empty, refresh it.
      loop livePorts [] ip2port | not $ Set.null livePorts = do
          let newPorts = Set.toList livePorts
          infoM "balance" $ "Reset live ports: " ++ show newPorts
          loop livePorts (Set.toList livePorts) ip2port

      -- Otherwise, proceed normally.
      loop livePorts portList@(nextPort:portTail) ip2port 
        | not $ Set.null livePorts = do
          infoM "balance" $ "next port: " ++ show nextPort
          infoM "balance" $ "port tail: " ++ show portTail
          -- Before taking any action, check whether the live TC's changed.
          svIsEmpty <- isEmptySV livePortsSV
          if not svIsEmpty then do
              livePorts' <- readSV livePortsSV
              -- Forget any dead ports and any IP addresses loaded onto
              -- dead ports.
              let portList' = filter (\p -> Set.member p livePorts') portList
              let ip2port' = Map.filter (\p -> Set.member p livePorts') ip2port
              infoM "balance" $ "livePorts': " ++ show livePorts'
              infoM "balance" $ "portList': " ++ show portList'
              loop livePorts' portList' ip2port'
          else return ()
          -- Otherwise, wait for a new flow.
          ((Loc switch port), pkt) <- readChan ipChan
          case pktNwSrc pkt of
            Nothing -> loop livePorts portList ip2port
            Just ip -> 
              if Map.member ip ip2port then loop livePorts portList ip2port
              else do
                  infoM "balance" $ "sending " ++ show ip ++ " to " ++ show nextPort
                  let ip2port' = Map.insert ip nextPort ip2port
                  writeChan polChan $ genPol ip2port' ipAct
                  loop livePorts portTail ip2port'

  -- Wait for the initial set of live ports.
  livePorts <- readSV livePortsSV
  forkIO $ loop livePorts (Set.toList livePorts) Map.empty
  writeChan polChan $ isIP ==> ipAct
  return polChan
  where
    genPol :: Map.Map IPAddress Port -> [Action] -> Policy
    genPol ip2port ipAct = genLoadRoutes ip2port <+> (genQ ip2port) ==> ipAct
    genLoadRoutes ip2port = Map.foldrWithKey f PoBottom ip2port <%> isIP
    f ip port pol = NwSrc (IPAddressPrefix ip 32) ==> forward [port] <+> pol
    genQ ip2port = Not $ Map.foldrWithKey g None ip2port
    g ip _ pred = NwSrc (IPAddressPrefix ip 32) <||> pred


-- Restrict policies in the channel to only match source IPs
-- in the given set of IP addresses.  Useful to ensure we only 
-- load-balance flows coming from the outside world.
restrict :: Set.Set IPAddress -> Chan Policy -> IO (Chan Policy)
restrict ips polChanIn = do
  polChanOut <- newChan
  let loop _ = do
      pol <- readChan polChanIn
      writeChan polChanOut $ pol <%> pred
      loop ()
  forkIO $ loop ()
  return polChanOut
  where
    pred = Set.fold f None ips
    f ip p = p <||> NwSrc (IPAddressPrefix ip 32)

transparentCache = do
  let sourceIPs = Set.fromList [ipAddress 10 0 0 1, ipAddress 10 0 0 2]
  let targetIPs = Set.fromList [ipAddress 10 0 0 3, ipAddress 10 0 0 4]
  (livePortsSV, heartbeatPol) <- monitorHeartbeats targetIPs
  loadChan <- balance livePortsSV
  restChan <- restrict sourceIPs loadChan
  allChan <- newChan
  let loop _ = do
      pol <- readChan restChan
      writeChan allChan $ pol 
                          <+> heartbeatPol 
                          <+> isArp ==> allPorts unmodified
      loop ()
  forkIO $ loop ()
  return allChan

startDebugLoop :: FilePath -> IO (Chan String)
startDebugLoop fp = do
  handle <- openFile fp WriteMode
  logChan <- newChan
  forkIO $ forever $ do
      str <- readChan logChan
      hPutStrLn handle str
      hFlush handle
  return logChan
  

main addr = do
  allChan <- transparentCache
  logChan <- startDebugLoop "transparentCacheDebugLog.log"
  debugDynController addr logChan allChan



