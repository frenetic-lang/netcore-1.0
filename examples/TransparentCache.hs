module TransparentCache where

import Control.Concurrent
import Control.Concurrent.MSampleVar
import Data.Set (Set)
import qualified Data.Set as Set
import Data.ByteString.Lazy (ByteString)
import Frenetic.NetCore
import Frenetic.NetworkFrames (arpReply)
import System.Log.Logger


-- Heartbeat --
-- For now, ping request packets count as the heartbeat.
isIP = DlTyp 0x800
heartbeatPred = isIP <&&> NwProto 1 <&&> NwTos 0
heartbeatDelay = 7 * 1000 * 1000 -- microseconds

-- Monitors for heartbeat packets from a given set of IP addresses and
-- periodically emits a new set of IPs that are still alive (i.e. have sent a
-- heartbeat in the most recent time interval).
monitorHeartbeats :: Set IPAddress -> IO (MSampleVar (Set IPAddress), Policy)
monitorHeartbeats targetIPs = do
  svar <- newEmptySV
  (pktChan, act) <- getPkts
  let pol = heartbeatPred ==> act
  -- In a loop, delay for heartbeatDelay milliseconds, then read all the
  -- heartbeat messages in the channel.  Any host that sent a message is
  -- still alive.
  let loop ips = do
      isEmpty <- isEmptyChan pktChan
      case isEmpty of
        False -> do
            heartbeatPkt <- readChan pktChan
            case getIP heartbeatPkt of
              Nothing -> loop ips
              Just ip
                | Set.member ip targetIPs -> loop $ Set.insert ip ips
                | otherwise -> loop ips
        True -> do
            writeSV svar ips
            threadDelay heartbeatDelay
            loop Set.empty
  let threadStart = do
      threadDelay heartbeatDelay
      loop Set.empty
  forkIO threadStart
  return (svar, pol)
  where
    getIP (_, Packet{..}) = pktNwSrc

printLiveIPs svar = do
  ips <- readSV svar
  infoM "heartbeat" $ show ips
  threadDelay 5000
  printLiveIPs svar

main = do
  let targetIPs = Set.fromList [ipAddress 10 0 0 1, ipAddress 10 0 0 2, ipAddress 10 0 0 3]
  (liveIPsSvar, heartbeatPol) <- monitorHeartbeats targetIPs
  forkIO $ printLiveIPs liveIPsSvar
  controller $ heartbeatPol <+> Any ==> allPorts unmodified

