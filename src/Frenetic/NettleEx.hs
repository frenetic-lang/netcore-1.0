-- |Nettle with additional features. None of this code is Frenetic-specific.
module Frenetic.NettleEx
  ( module Nettle.OpenFlow
  , ethVLANId
  , ethVLANPcp
  , ethSrcIP
  , ethDstIP
  , ethProto
  , ethTOS
  , srcPort
  , dstPort
  ) where

import Frenetic.Common
import qualified Data.Map as Map
import Nettle.OpenFlow hiding (intersect)
import qualified Nettle.Servers.Server as Server
import Nettle.Servers.Server
import Nettle.Ethernet.EthernetFrame
import Nettle.Ethernet.AddressResolutionProtocol
import Prelude hiding (catch)
import Control.Exception

csMsgWithResponse :: CSMessage -> Bool
csMsgWithResponse msg = case msg of
  CSHello -> True
  CSEchoRequest _ -> True
  FeaturesRequest -> True
  StatsRequest _ -> True
  BarrierRequest -> True
  GetQueueConfig _ -> True
  otherwise -> False

hasMoreReplies :: SCMessage -> Bool
hasMoreReplies msg = case msg of
  StatsReply (FlowStatsReply True _) -> True
  StatsReply (TableStatsReply True _) -> True
  StatsReply (PortStatsReply True _) -> True
  StatsReply (QueueStatsReply True _) -> True
  otherwise -> False

ethVLANId :: EthernetHeader -> Maybe VLANID
ethVLANId (Ethernet8021Q _ _ _ _ _ vlanId) = Just vlanId
ethVLANId (EthernetHeader {}) = Nothing

ethVLANPcp :: EthernetHeader -> VLANPriority
ethVLANPcp (EthernetHeader _ _ _) = 0
ethVLANPcp (Ethernet8021Q _ _ _ pri _ _) = pri

stripIP (IPAddress a) = a

ethSrcIP (IPInEthernet (HCons hdr _)) = Just (stripIP (ipSrcAddress hdr))
ethSrcIP (ARPInEthernet (ARPQuery q)) = Just (stripIP (querySenderIPAddress q))
ethSrcIP (ARPInEthernet (ARPReply r)) = Just (stripIP (replySenderIPAddress r))
ethSrcIP (UninterpretedEthernetBody _) = Nothing

ethDstIP (IPInEthernet (HCons hdr _)) = Just (stripIP (ipDstAddress hdr))
ethDstIP (ARPInEthernet (ARPQuery q)) = Just (stripIP (queryTargetIPAddress q))
ethDstIP (ARPInEthernet (ARPReply r)) = Just (stripIP (replyTargetIPAddress r))
ethDstIP (UninterpretedEthernetBody _) = Nothing

ethProto (IPInEthernet (HCons hdr _)) = Just (ipProtocol hdr)
ethProto (ARPInEthernet (ARPQuery _)) = Just 1
ethProto (ARPInEthernet (ARPReply _)) = Just 2
ethProto (UninterpretedEthernetBody _) = Nothing

ethTOS (IPInEthernet (HCons hdr _)) = Just (dscp hdr)
ethTOS _ = Just 0

srcPort (IPInEthernet (HCons _ (HCons pk _))) = case pk of
  TCPInIP (src, dst) -> Just src
  UDPInIP (src, dst) _ -> Just src
  ICMPInIP (typ, cod) -> Just (fromIntegral typ)
  UninterpretedIPBody _ -> Nothing
srcPort _ = Nothing

dstPort (IPInEthernet (HCons _ (HCons pk _))) = case pk of
  TCPInIP (src, dst) -> Just dst
  UDPInIP (src, dst) _ -> Just dst
  ICMPInIP (typ, cod) -> Just (fromIntegral cod)
  UninterpretedIPBody _ -> Nothing
dstPort _ = Nothing
