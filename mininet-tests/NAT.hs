-- ^Uses NetCore to implement a Network Address Translator
--
-- Does not work on networks with loops.
module NAT where

import Control.Concurrent
import Control.Monad (forever)
import Frenetic.NetCore
import Frenetic.NetCore.Types
import Frenetic.EthernetAddress
import qualified Data.Map as Map
import Data.Word
import Data.Bits


-- Shorthand for hard-coded IP addresses
addr :: (Word8, Word8, Word8, Word8) -> Word32
addr (a, b, c, d) = 
  let a' = fromIntegral a
      b' = fromIntegral b
      c' = fromIntegral c
      d' = fromIntegral d
    in shift a' 24 .|. shift b' 16 .|. shift c' 8 .|. d'

-- MAC address of the gateway box
gateMAC = ethernetAddress 0x00 0x00 0x00 0x00 0x00 0x11

-- Fake "NAT" IP address
fakeIP = addr (10,0,0,101)

-- Internal IP traffic
private = nwDstPrefix 0x0a000000 24

-- ARP traffic
arp = dlTyp 0x0806

-- Traffic directed to the gateway
toGateway = dlDst gateMAC

-- Port to gateway
gatePort = 3

-- Traffic arriving from the gateway
fromGateway = inPort gatePort

-- Returns a pair of two channels: the first is a channel of unique packets
-- (unique tuples of MAC address, location, and source TCP port) that have
-- been sent to the gateway.  The second is a channel of policies that
-- implement the packet query---as new tuples are recognized, the policy is
-- refined to exclude more of the same packet.
pktsByLocation :: IO (Chan Packet, Chan Policy)
pktsByLocation = do
  uniqPktChan <- newChan
  polChan <- newChan
  (pktChan, act) <- getPkts
  let loop :: Map.Map (Word32, Port) (Word48, Predicate) -> IO ()
      loop locs = do
        (sw, pkt) <- readChan pktChan
        let srcMac = pktDlSrc pkt
        let srcIp = pktNwSrc pkt
        let srcTp = pktTpSrc pkt
        case (srcIp, srcTp) of
          (Just srcIp, Just srcTp) -> do
            case Map.lookup (srcIp, srcTp) locs of
              Just (srcMac',_) | srcMac == srcMac' -> do
                -- Not a new location, nothing to do.
                loop locs
              otherwise -> do
                -- Either a new outgoing stream or a new MAC address associated
                -- with an existing IP (we're kind of simulating ARP here...)
                let pred = dlSrc srcMac <&&> nwSrc srcIp <&&> tpSrc srcTp
                let locs' = Map.insert (srcIp, srcTp) (srcMac, pred) locs
                writeChan uniqPktChan pkt
                writeChan polChan $
                  (dlDst gateMAC) <&&>
                  (neg (prOr (map snd (Map.elems locs')))) ==> act
                loop locs'
          otherwise -> loop locs
  -- Initially, inspect all packets destined for the gateway.
  writeChan polChan (dlDst gateMAC ==> act)
  forkIO (loop Map.empty)
  return (uniqPktChan, polChan)

-- Implement NAT logic here.
-- 
--  This version of a NAT simply maps internal (IP, Port) pairs to
--  a unique externap port.  We implement this with modification actions
--  on the switch:
--
--      1: Internal -> External.  If dlDst = 0x1, then the packet is bound
--         for the outside world.  Replace dlDst with 0x11, nwSrc with fakeIP,
--         and tpSrc with the mapping from (srcIP, tpSrc).
--
--      2: External -> Internal.  Packets arriving on port 3 (i.e. from the 
--         outside world) will have dstIP = fakeIP and dstPort = map(srcIP, tpSrc).
--         Replace with the proper IP, port, and MAC.
installHosts :: Chan Packet -> IO (Chan Policy)
installHosts pktChan = do
  polChan <- newChan
  -- Map (SrcIP, SrcPort) to an external port coupled with two policies;
  -- for outgoing and incoming packets, respectively.
  let loop :: Map.Map (Word32, Port) (Port, Policy) -> Port -> IO ()
      loop locs pNum = do
        pkt <- readChan pktChan
        -- Extract the internal source information from the packet.
        let srcMac = pktDlSrc pkt
        let srcIp = pktNwSrc pkt
        let srcTp = pktTpSrc pkt
        let inSwitchPort = pktInPort pkt
        -- Ignore this packet if the source IP or port aren't set.
        case (srcIp, srcTp) of
            (Just srcIp, Just srcTp) -> do
              -- Map (srcIp, srcTp) to a unique external port, picking a new one if
              -- one doesn't already exist.
              let (extPort, pNum) = case Map.lookup (srcIp, srcTp) locs of {
                  Just (ePort, _) -> (ePort, pNum);
                  Nothing -> (pNum, pNum + 1) }
              -- Create the policy that rewrites the internal source on outgoing packets.
              let outMod = (modNwSrc fakeIP) {modifyTpSrc = Just extPort}
              let outPol = dlSrc srcMac 
                      <&&> dlDst gateMAC
                      <&&> nwSrc srcIp
                      <&&> tpSrc srcTp
                       ==> modify [(gatePort, outMod)]
              let outPol = dlDst gateMAC ==> forward [gatePort]
              -- Create the policy that replaces the internal source on incoming packets.
              let inMod  = (modNwDst srcIp) {modifyTpDst = Just srcTp, modifyDlSrc = Just srcMac}
              let inPol  = nwDst srcIp
                      <&&> tpDst extPort
                       ==> modify [(inSwitchPort, inMod)]
              -- Update the map.
              let locs' = Map.insert (srcIp, srcTp) (extPort, inPol <+> outPol) locs
              -- Push the new policy.
              let newPol = foldr1 (<+>) $ map (\(_,(_,pol))->pol) $ Map.toList locs'
              writeChan polChan newPol
              loop locs' pNum
            otherwise -> loop locs pNum
  writeChan polChan $ (dlDst gateMAC ==> forward [gatePort]) <+> (nwDst fakeIP ==> dropPkt)
  forkIO (loop Map.empty 1)
  return polChan


nonNATPolicy :: Policy
nonNATPolicy = 
  -- ARP
  ((arp ==> allPorts unmodified) <+>
  -- Internal traffic
  (private ==> allPorts unmodified)) <%>
  -- Restricted to traffic not destined for the gateway
  (neg $ dlDst gateMAC)


nat :: IO (Chan Policy)
nat = do
  (uniqPktsChan, queryPolChan) <- pktsByLocation
  fwdPolChan <- installHosts uniqPktsChan
  bothPolsChan <- both queryPolChan fwdPolChan
  polChan <- newChan
  forkIO $ forever $ do
    (queryPol, fwdPol) <- readChan bothPolsChan
    writeChan polChan (nonNATPolicy <+> fwdPol <+> queryPol)
  return polChan


main = do
  polChan <- nat
  pktChan <- newChan
  dynController polChan pktChan

