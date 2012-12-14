module Arp (doArp, main) where

import Control.Concurrent
import qualified Data.Map as Map
import Data.ByteString.Lazy (ByteString)
import Data.Word
import Frenetic.NetCore
import Frenetic.NetworkFrames (arpReply)
import MacLearning (learningSwitch)
import System.Log.Logger

type IpMap = Map.Map IPAddress EthernetAddress

-- See RFC 826 for ARP specification
-- Note that the OpenFlow spec uses the Nw* fields for both IP packets and ARP
-- packets, which is how nwProto matches the ARP query/reply field.
-- Ethertype for ARP packets
isArp = DlTyp 0x0806
-- Numbers from ARP protocol
isQuery = isArp <&&> NwProto 1
isReply = isArp <&&> NwProto 2

known ips = prOr . map (\ip -> NwDst (IPAddressPrefix ip 32)) $ Map.keys ips

-- |Maybe produce a reply packet from an ARP request.
-- Try to look up the IP address in the lookup table.  If there's an associated
-- MAC address, build an ARP reply packet, otherwise return Nothing.  It's
-- possible that we get a malformed packet that doesn't have one or more of the
-- IP src or dst fields, in which case also return Nothing.
handleQuery :: LocPacket -> IpMap -> Maybe (Loc, ByteString)
handleQuery (Loc switch port, Packet {..}) ips =
  case pktNwDst of
    Nothing -> Nothing
    Just toIp ->
      case Map.lookup toIp ips of
        Nothing -> Nothing
        Just toMac ->
          case pktNwSrc of
            Nothing -> Nothing
            Just fromIp -> Just (Loc switch port,
                                 arpReply toMac 
                                   (ipAddressToWord32 toIp) 
                                   pktDlSrc 
                                   (ipAddressToWord32 fromIp))

-- |Maybe produce a new IP address map from an ARP reply.
-- Try to parse the packet, and if you can, and it's a new mapping, return the
-- new map.  Otherwise, return Nothing to signal that we didn't learn something
-- new so it's not necessary to update the policy.
handleReply :: LocPacket -> IpMap -> Maybe IpMap
handleReply (Loc switch _, packet) ips =
  let fromMac = pktDlSrc packet in
  case pktNwSrc packet of
    Nothing -> Nothing
    Just fromIp ->
      -- If we already know about the MAC address of the reply, do nothing.
      if Map.member fromIp ips then Nothing
      -- Otherwise, update the IP address map
      else Just (Map.insert fromIp fromMac ips)

-- |Use the controller as an ARP cache with an underlying routing policy.
-- This is parameterized on a channel that yields policies that provide basic
-- host-to-host connectivity.  The simplest implementation simplhy floods
-- packets, but more sophisticated options might include a learning switch, or a
-- topology-based shortest path approach.
doArp :: Chan Policy -> IO (Chan Policy)
doArp routeChan = do
  (queryChan, queryAction) <- getPkts
  (replyChan, replyAction) <- getPkts
  dataChan <- select queryChan replyChan
  allChan <- select routeChan dataChan
  packetOutChan <- newChan
  policyOutChan <- newChan
  -- The overall architecture of the loop:
  -- * allChan aggregates everything that may cause us to update.  It's either
  -- ** A new routing policy for the underlying connectivity,
  -- ** An ARP query packet, or
  -- ** An ARP reply packet.
  --
  -- During the loop, we need to track two pieces of data:
  -- * The current underlying connectivity policy, which may be Nothing if we
  --   haven't learned one yet, and
  -- * A map from IP addresses to MAC addresses, which stores the ARP lookups
  --   which we already know about.
  --
  -- Ultimately, we need to generate a new routing and packet query  policy that
  -- routes the ARP packets we're not going to handle, plus installs the packet
  -- receiving actions to query the packets we want to either reply to or learn
  -- from.
  let buildPolicy route ips = route' <+> query <+> reply <+> 
                              SendPackets packetOutChan
        where
        -- First, we define whether we can reply to a particular packet as
        -- whether it is an ARP query (thus, it wants a reply) for an IP address
        -- which we've already learned the MAC address for.
        canReply = isQuery <&&> known ips
        -- We want to provide connectivity for all packets except the ones we're
        -- going to reply to directly.  We want to drop those because we're
        -- going to handle them, so there's no use in them reaching other hosts.
        route' = route <%> Not canReply
        -- The corollary to this is that we need to query all the packets that
        -- we can reply to so that the controller can send an ARP packet their
        -- way.
        query = canReply ==> queryAction
        -- We also want to spy on (but not impede forwarding of) ARP reply
        -- packets so that we can learn the sender's IP and MAC addresses,
        -- letting us reply from the controller next time.
        reply = isReply <&&> Not (known ips) ==> replyAction
  let loop mRoute ips = do
      update <- readChan allChan
      case update of
        Left route -> do
          -- If we get a new connectivity policy in, we recompile our overall
          -- policy, send that out, and recurse on the new route.
          infoM "ARP" "Got new routing policy for ARP"
          writeChan policyOutChan (buildPolicy route ips)
          loop (Just route) ips
        Right arpData ->
          case mRoute of
            -- If we don't have a routing policy yet, we're not even providing
            -- connectivity, so it's probably a spurious packet and we can just
            -- ignore it.
            Nothing -> loop Nothing ips
            Just route ->
              case arpData of
                Left query ->
                  -- If we get an ARP query, try to form a response to it.  If
                  -- we can, send the packet out, otherwise do nothing.  Either
                  -- way, recurse.
                  case handleQuery query ips of
                    Nothing -> loop mRoute ips
                    Just packet -> do
                      infoM "ARP" $ "Sending ARP reply: " ++ show packet
                      writeChan packetOutChan packet
                      loop mRoute ips
                Right reply ->
                  -- If we get an ARP reply, try to learn from it.  If we
                  -- learned a new mapping, update the routing and query policy
                  -- to reflect the fact that we're going to handle these
                  -- packets in the future and recurse on the new mapping.
                  -- Otherwise, just recurse.
                  case handleReply reply ips of
                    Nothing -> loop mRoute ips
                    Just ips' -> do
                      infoM "ARP" $ "Learned ARP route from " ++ show reply
                      writeChan policyOutChan (buildPolicy route ips')
                      loop mRoute ips'
  forkIO (loop Nothing Map.empty)
  return policyOutChan

-- |Runs ARP on top of a learning switch to gradually learn both routes and the
-- ARP table.  Provides both regular connectivity and synthetic ARP replies.
main addr = do
  lsChan <- learningSwitch
  policyChan <- doArp lsChan
  dynController addr policyChan
