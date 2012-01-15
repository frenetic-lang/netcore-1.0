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
-- /src/Network.hs                                                            --
-- Frenetic network types                                                     --
-- $Id$ --
--------------------------------------------------------------------------------

{-# LANGUAGE 
    NoMonomorphismRestriction,
    ExistentialQuantification,
    MultiParamTypeClasses,
    StandaloneDeriving,
    GADTs
 #-}

module Frenetic.Network where

import Numeric
import Data.Char 
import Data.Word
import Nettle.OpenFlow.Packet
import Nettle.Ethernet.EthernetFrame
import Nettle.Ethernet.EthernetAddress
import Nettle.IPv4.IPPacket
import Nettle.IPv4.IPAddress
--
-- Basic types
-- 
type Switch = Word64
type Port = Word16
data HardwareAddress = HardwareAddress EthernetAddress
  deriving Eq

instance Show HardwareAddress where
  show (HardwareAddress (EthernetAddress w1 w2 w3 w4 w5 w6)) = 
    let hex x = (if x < 10 then "0" else "") ++ showHex x "" in 
    (hex w1) ++ ":" ++ (hex w2) ++ ":" ++ (hex w3) ++ ":" ++ 
    (hex w4) ++ ":" ++ (hex w5) ++ ":" ++ (hex w6)

-- 
-- Headers
-- 
data Header b where
  Dl_src :: Header HardwareAddress
  Dl_dst :: Header HardwareAddress
  Dl_typ :: Header Word16
  Dl_vlan :: Header Word16
  Dl_vlan_pcp :: Header Word8
  Nw_src :: Header Word32
  Nw_dst :: Header Word32
  Nw_proto :: Header Word8
  Nw_tos :: Header Word8
  Tp_src :: Header Word16
  Tp_dst :: Header Word16

deriving instance Eq b => Eq (Header b)
deriving instance Show b => Show (Header b)
deriving instance Ord b => Ord (Header b)

--
-- Packets
--
class (Eq p) => Packet p where
  getHeader :: p -> Header b -> b
--   setHeader :: p -> Header b -> b -> p

data ConcretePacket = 
  ConcretePacket 
    { dl_src :: HardwareAddress, 
      dl_dst :: HardwareAddress,
      dl_typ :: Word16,
      dl_vlan :: Word16,
      dl_vlan_pcp :: Word8,
      nw_src :: Word32,
      nw_dst :: Word32,
      nw_typ :: Word16,
      nw_proto :: Word8,
      nw_tos :: Word8,
      tp_src :: Word16,
      tp_dst :: Word16 }
  deriving Eq

instance Packet ConcretePacket where
  getHeader cp h = 
    case h of 
      Dl_src -> dl_src cp 
      Dl_dst -> dl_dst cp
      Dl_typ -> dl_typ cp
      Dl_vlan -> dl_vlan cp 
      Dl_vlan_pcp -> dl_vlan_pcp cp
      Nw_src -> nw_src cp
      Nw_dst -> nw_dst cp
      Nw_proto -> nw_proto cp
      Nw_tos -> nw_tos cp
      Tp_src -> tp_src cp
      Tp_dst -> tp_dst cp
--   setHeader cp h v = 
--     case h of 
--       Dl_src -> cp { dl_src = v }
--       Dl_dst -> cp { dl_dst = v }
--       Dl_typ -> cp { dl_typ = v }
--       Dl_vlan -> cp {dl_vlan = v }
--       Dl_vlan_pcp -> cp { dl_vlan_pcp = v }
--       Nw_src -> cp { nw_src = v }
--       Nw_dst -> cp { nw_dst = v }
--       Nw_proto -> cp { nw_proto = v }
--       Nw_tos -> cp { nw_tos = v }
--       Tp_src -> cp { tp_src = v }
--       Tp_dst -> cp { tp_dst = v }

--
-- Nettle instances
--
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

--
-- Transmissions
--
data Transmission p = forall p. (Packet p) => Transmission Switch Port p
