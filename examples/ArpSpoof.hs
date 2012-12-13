module ArpSpoof (spoofSwitchArp, main) where

import Control.Concurrent
import Data.ByteString.Lazy (ByteString)
import Frenetic.NetCore
import Frenetic.NetworkFrames (arpReply)


-- Heartbeat --

-- Give the server an Ip address by monitoring ARP requests and
-- spoofing replies.
isArp = DlTyp 0x806
isQuery = isArp <&&> NwProto 1
isReply = isArp <&&> NwProto 2

-- Mac and Ip addresses assigned to the server.
serverMac = ethernetAddress 0x00 0x00 0x00 0x00 0x00 0x11
serverIp = ipAddress 10 0 0 101


-- Return a policy that queries all ARP request packets
-- destined for serverIp and, upon receiving such a packet, 
-- injects an appropriate response.
spoofSwitchArp :: Switch -> EthernetAddress -> IPAddress -> IO (Policy)
spoofSwitchArp switchId serverMac serverIp = do
  (arpQueryChan, arpAct) <- getPkts
  packetOutChan <- newChan
  let pol = isQuery <&&> NwDst (IPAddressPrefix serverIp 32) ==> arpAct <+> SendPackets packetOutChan
  let loop a = do
      arpQueryPkt <- readChan arpQueryChan
      case generateReply arpQueryPkt of
        Nothing -> loop a 
        Just replyPkt -> do
            writeChan packetOutChan replyPkt
            loop a
  forkIO $ loop True
  return $ pol <%> Switch switchId
  where
    -- Given an ARP query packet asking for the server, generate a response.  If
    -- the located packet is malformed---eg. without a valid nwSrc field---return
    -- Nothing.
    generateReply :: LocPacket -> Maybe (Loc, ByteString)
    generateReply (Loc switch port, Packet {..}) = 
      case pktNwSrc of
        Nothing -> Nothing
        Just fromIp -> Just (Loc switch port,
                             arpReply 
                               serverMac (ipAddressToWord32 serverIp)
                               pktDlSrc (ipAddressToWord32 fromIp))

main addr = do
  arpSpoof <- spoofSwitchArp 1 serverMac serverIp
  controller addr $ arpSpoof <+> Any ==> allPorts unmodified

