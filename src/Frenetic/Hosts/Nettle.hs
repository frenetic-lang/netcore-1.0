
--------------------------------------------------------------------------------
-- The Frenetic Project                                                       --
-- frenetic@frenetic-lang.org                                                 --
--------------------------------------------------------------------------------
-- Licensed to the Frenetic Project by one or more contributors. See the      --
-- NOTICE file distributed with this work for additional information          --
-- regarding copyright and ownership. The Frenetic Project licenses this      --
-- file to you under the following license.                                   --
--                                                                            --
-- Redistribution and use in source and binary forms, with or without         --
-- modification, are permitted provided the following conditions are met:     --
-- * Redistributions of source code must retain the above copyright           --
--   notice, this list of conditions and the following disclaimer.            --
-- * Redistributions of binaries must reproduce the above copyright           --
--   notice, this list of conditions and the following disclaimer in          --
--   the documentation or other materials provided with the distribution.     --
-- * The names of the copyright holds and contributors may not be used to     --
--   endorse or promote products derived from this work without specific      --
--   prior written permission.                                                --
--                                                                            --
-- Unless required by applicable law or agreed to in writing, software        --
-- distributed under the License is distributed on an "AS IS" BASIS, WITHOUT  --
-- WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the   --
-- LICENSE file distributed with this work for specific language governing    --
-- permissions and limitations under the License.                             --
--------------------------------------------------------------------------------
-- src/Frenetic/Hosts/Nettle.hs                                                            --
-- Nettle Host                                                   --
-- $Id$ --
--------------------------------------------------------------------------------

{-# LANGUAGE
    NoMonomorphismRestriction,
    StandaloneDeriving,
    FlexibleInstances,
    Rank2Types,
    GADTs,
    ExistentialQuantification
 #-}

module Frenetic.Hosts.Nettle where

import Nettle.OpenFlow.Packet
import Nettle.Ethernet.EthernetFrame
import Nettle.Ethernet.EthernetAddress
import Nettle.IPv4.IPPacket
import Nettle.IPv4.IPAddress

import Frenetic.Language
import Frenetic.Network
    

-- Packet instances    

nettleEthernetFrame pkt = 
  case runGetE getEthernetFrame $ packetData pkt of 
    Left err -> error ("Expected an Ethernet frame: " ++ err)
    Right ef -> ef

nettleEthernetHeaders pkt = 
  case nettleEthernetFrame pkt of
    EthernetFrame hdr _ -> hdr

nettleEthernetBody pkt = 
  case nettleEthernetFrame pkt of
    EthernetFrame _ bdy -> bdy

instance Packet Nettle.OpenFlow.Packet.PacketInfo where 
  getHeader pi h = 
    case h of 
      Dl_src -> 
        HardwareAddress $ sourceMACAddress $ nettleEthernetHeaders pi
      Dl_dst -> 
        HardwareAddress $ destMACAddress $ nettleEthernetHeaders pi
      Dl_typ -> 
        typeCode $ nettleEthernetHeaders pi
      Dl_vlan -> 
        case nettleEthernetHeaders pi of
          EthernetHeader _ _ _ -> 0xfffff 
          Ethernet8021Q _ _ _ _ _ vlan -> vlan
      Dl_vlan_pcp -> 
        case nettleEthernetHeaders pi of
          EthernetHeader _ _ _ -> 0 
          Ethernet8021Q _ _ _ pri _ _ -> pri
      Nw_src -> 
        case addr of IPAddress a -> a
        where addr = case nettleEthernetBody pi of
                       IPInEthernet (IPPacket hdr _) -> ipSrcAddress hdr
                       ARPInEthernet arp -> senderIPAddress arp
                       _ -> ipAddress 0 0 0 0
      Nw_dst -> 
        case addr of IPAddress a -> a
        where addr = case nettleEthernetBody pi of
                       IPInEthernet (IPPacket hdr _) -> ipDstAddress hdr
                       ARPInEthernet arp -> targetIPAddress arp
                       _ -> ipAddress 0 0 0 0
      Nw_proto -> 
        case nettleEthernetBody pi of
          IPInEthernet (IPPacket hdr _) -> ipProtocol hdr
          ARPInEthernet arp -> 
            case arpOpCode arp of 
              ARPRequest -> 1
              ARPReply -> 2
          _ -> 0
      Nw_tos -> 
          case nettleEthernetBody pi of
            IPInEthernet (IPPacket hdr _) -> dscp hdr
            _ -> 0 
      Tp_src -> 
        case nettleEthernetBody pi of 
          IPInEthernet (IPPacket _ (TCPInIP (src,_))) -> src
          IPInEthernet (IPPacket _ (UDPInIP (src,_))) -> src
          IPInEthernet (IPPacket _ (ICMPInIP (typ,_))) -> fromIntegral typ
          _ -> 0
      Tp_dst ->
        case nettleEthernetBody pi of 
          IPInEthernet (IPPacket _ (TCPInIP (_,dst))) -> dst
          IPInEthernet (IPPacket _ (UDPInIP (_,dst))) -> dst
          IPInEthernet (IPPacket _ (ICMPInIP (_,cod))) -> fromIntegral cod
          _ -> 0
