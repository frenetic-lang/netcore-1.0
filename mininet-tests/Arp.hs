module Arp (main) where

import Control.Concurrent
import qualified Data.Map as Map
import Data.ByteString.Lazy (ByteString)
import Data.Word
import Frenetic.Common (mergeChan)
import Frenetic.NetCore
import Frenetic.NetworkFrames (arpReply)
import MacLearning (learningSwitch)

type IpMap = Map.Map Word32 EthernetAddress

-- See RFC 826 for ARP specification
-- Note that the OpenFlow spec uses the Nw* fields for both IP packets and ARP
-- packets, which is how nwProto matches the ARP query/reply field.
-- Ethertype for ARP packets
isArp = dlTyp 0x0806
-- Numbers from ARP protocol
isQuery = isArp <&&> nwProto 1
isReply = isArp <&&> nwProto 2

known ips = prOr . map (\ipAddr -> nwDst ipAddr) $ Map.keys ips

-- |Maybe produce a reply packet from an ARP request
handleQuery :: (Switch, Packet) -> IpMap -> Maybe (Loc, ByteString)
handleQuery (switch, Packet {..}) ips =
  case pktNwDst of
    Nothing -> Nothing -- TODO(astory): log
    Just toIp ->
      case Map.lookup toIp ips of
        Nothing -> Nothing
        Just toMac ->
          case pktNwSrc of
            Nothing -> Nothing -- TODO(astory): log
            Just fromIp -> Just (Loc switch pktInPort,
                                 arpReply toMac toIp pktDlSrc fromIp)

-- |Maybe produce a new IP address map from an ARP reply
handleReply :: (Switch, Packet) -> IpMap -> Maybe IpMap
handleReply (switch, packet) ips =
  let fromMac = pktDlSrc packet in
  case pktNwSrc packet of
    Nothing -> Nothing -- TODO(astory): log
    Just fromIp ->
      -- If we already know about the MAC address of the reply, do nothing.
      if Map.member fromIp ips then Nothing
      -- Otherwise, update the IP address map
      else Just (Map.insert fromIp fromMac ips)

-- |Use the controller as an ARP cache with an underlying routing policy
doArp :: Chan Policy -> IO (Chan Policy, Chan (Loc, ByteString))
doArp routeChan = do
  (queryChan, queryAction) <- getPkts
  (replyChan, replyAction) <- getPkts
  dataChan <- mergeChan queryChan replyChan
  allChan <- mergeChan routeChan dataChan
  packetOutChan <- newChan
  policyOutChan <- newChan
  let buildPolicy route ips = route' <+> query <+> reply
        where
        route' = route <%> neg canReply
        query = canReply ==> queryAction
        reply = isReply <&&> neg (known ips) ==> replyAction
        canReply = isQuery <&&> known ips
  let loop mRoute ips = do
      update <- readChan allChan
      case update of
        Left route -> do
          -- Don't route packets that we know how to reply to
          writeChan policyOutChan (buildPolicy route ips)
          loop (Just route) ips
        Right arpData -> do
          case mRoute of
            -- If we don't have a routing policy yet, just drop it
            Nothing -> loop Nothing ips
            Just route ->
              case arpData of
                Left query ->
                  case handleQuery query ips of
                    Nothing -> loop mRoute ips
                    Just packet -> do
                      writeChan packetOutChan packet
                      loop mRoute ips
                Right reply ->
                  case handleReply reply ips of
                    Nothing -> loop mRoute ips
                    Just ips' -> do
                      writeChan policyOutChan (buildPolicy route ips')
                      loop mRoute ips'
  forkIO (loop Nothing Map.empty)
  return (policyOutChan, packetOutChan)

main = do
  lsChan <- learningSwitch
  (policyChan, packetChan) <- doArp lsChan
  dynController policyChan packetChan
