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
-- OpenFlow classifiers                                                       --
-- $Id$ --
--------------------------------------------------------------------------------

{-# LANGUAGE 
    NoMonomorphismRestriction,
    GADTs,
    TypeSynonymInstances,
    FlexibleInstances,
    MultiParamTypeClasses,
    StandaloneDeriving,
    DeriveDataTypeable
 #-}

module Frenetic.Switches.OpenFlow where

import           Control.Newtype

import           Data.Bits
import           Frenetic.LargeWord
import qualified Data.Set                        as Set
import           Data.Typeable
import           Data.Word


import           Nettle.OpenFlow.Packet
import qualified Nettle.OpenFlow.Match           as OFMatch
import qualified Nettle.OpenFlow.Action          as OFAction
import qualified Nettle.IPv4.IPPacket            as IPPacket
import qualified Nettle.IPv4.IPAddress           as IPAddress
import           Nettle.Ethernet.EthernetFrame
import           Nettle.Ethernet.EthernetAddress



    


import           Frenetic.Pattern
import           Frenetic.Language
import           Frenetic.Compiler


{-| Convert an EthernetAddress to a Word48. -}    
ethToWord48 (EthernetAddress a b c d e f) =
    LargeKey a (LargeKey b (LargeKey c (LargeKey d (LargeKey e f))))
             
{-| Convert a Word48 to an EthernetAddress. -}    
word48ToEth (LargeKey a (LargeKey b (LargeKey c (LargeKey d (LargeKey e f))))) =
    EthernetAddress a b c d e f

{-| Convert a pattern Prefix to an IPAddressPrefix. -}
prefixToIPAddressPrefix :: Prefix Word32 -> IPAddress.IPAddressPrefix
prefixToIPAddressPrefix (Prefix (Wildcard x m)) =
    (IPAddress.IPAddress x, fromIntegral $ length $ filter (testBit m) [0 .. 31])

{-| Convert an IPAddressPrefix to a pattern Prefix. -}
ipAddressPrefixToPrefix :: IPAddress.IPAddressPrefix -> Prefix Word32
ipAddressPrefixToPrefix (IPAddress.IPAddress x, len) = 
  Prefix (Wildcard x (foldl (\m i -> setBit m (31 - i)) 0 [0 .. fromIntegral len - 1]))

instance Matchable IPAddress.IPAddressPrefix where
  top = IPAddress.defaultIPPrefix
  intersect = IPAddress.intersect

instance GAction OFAction.ActionSequence where
    actnController = OFAction.sendToController 0
    actnDefault = OFAction.sendToController 0
    actnTranslate s = map (OFAction.SendOutPort . OFAction.PhysicalPort) $ Set.toList s

deriving instance Typeable OFMatch.Match

instance Matchable OFMatch.Match where
  top = OFMatch.Match { 
          OFMatch.inPort = top,
          OFMatch.srcEthAddress = top,
          OFMatch.dstEthAddress = top,
          OFMatch.vLANID = top,
          OFMatch.vLANPriority = top,
          OFMatch.ethFrameType = top,
          OFMatch.ipTypeOfService = top,
          OFMatch.ipProtocol = top,
          OFMatch.srcIPAddress = top,
          OFMatch.dstIPAddress = top,
          OFMatch.srcTransportPort = top,
          OFMatch.dstTransportPort = top }
        
  intersect ofm1 ofm2 = 
      do inport <- intersect (OFMatch.inPort ofm1) (OFMatch.inPort ofm2)
         srcethaddress <- intersect (OFMatch.srcEthAddress ofm1) (OFMatch.srcEthAddress ofm2)
         dstethaddress <- intersect (OFMatch.dstEthAddress ofm1) (OFMatch.dstEthAddress ofm2)
         vlanid <- intersect (OFMatch.vLANID ofm1) (OFMatch.vLANID ofm2)
         vlanpriority <- intersect (OFMatch.vLANPriority ofm1) (OFMatch.vLANPriority ofm2)
         ethframetype <- intersect (OFMatch.ethFrameType ofm1) (OFMatch.ethFrameType ofm2)
         iptypeofservice <- intersect (OFMatch.ipTypeOfService ofm1) (OFMatch.ipTypeOfService ofm2)
         ipprotocol <- intersect (OFMatch.ipProtocol ofm1) (OFMatch.ipProtocol ofm2)
         srcipaddress <- intersect (OFMatch.srcIPAddress ofm1) (OFMatch.srcIPAddress ofm2)
         dstipaddress <- intersect (OFMatch.dstIPAddress ofm1) (OFMatch.dstIPAddress ofm2)
         srctransportport <- intersect (OFMatch.srcTransportPort ofm1) (OFMatch.srcTransportPort ofm2)
         dsttransportport <- intersect (OFMatch.dstTransportPort ofm1) (OFMatch.dstTransportPort ofm2)
         return $ OFMatch.Match { 
           OFMatch.inPort = inport,
           OFMatch.srcEthAddress = srcethaddress,
           OFMatch.dstEthAddress = dstethaddress,
           OFMatch.vLANID = vlanid,
           OFMatch.vLANPriority = vlanpriority,
           OFMatch.ethFrameType = ethframetype,
           OFMatch.ipTypeOfService = iptypeofservice,
           OFMatch.ipProtocol = ipprotocol,
           OFMatch.srcIPAddress = srcipaddress,
           OFMatch.dstIPAddress = dstipaddress,
           OFMatch.srcTransportPort = srctransportport,
           OFMatch.dstTransportPort = dsttransportport }


instance GPattern OFMatch.Match where
  fromPatternOverapprox ptrn = top {
    OFMatch.srcEthAddress = fmap word48ToEth $ overapprox $ ptrnDlSrc ptrn,
    OFMatch.dstEthAddress = fmap word48ToEth $ overapprox $ ptrnDlDst ptrn,
    OFMatch.ethFrameType = overapprox $ ptrnDlTyp ptrn,
    OFMatch.vLANID = overapprox $ ptrnDlVlan ptrn,
    OFMatch.vLANPriority = overapprox $ ptrnDlVlanPcp ptrn,
    OFMatch.srcIPAddress = prefixToIPAddressPrefix $ overapprox $ ptrnNwSrc ptrn ,
    OFMatch.dstIPAddress = prefixToIPAddressPrefix $ overapprox $ ptrnNwDst ptrn ,
    OFMatch.ipProtocol = overapprox $ ptrnNwProto ptrn,
    OFMatch.ipTypeOfService = overapprox $ ptrnNwTos ptrn,
    OFMatch.srcTransportPort = overapprox $ ptrnTpSrc ptrn,
    OFMatch.dstTransportPort = overapprox $ ptrnTpDst ptrn, 
    OFMatch.inPort = ptrnInPort ptrn
    }
    
  fromPatternUnderapprox pkt ptrn = do 
    ptrnDlSrc' <- underapprox (ptrnDlSrc ptrn) (pktDlSrc pkt)
    ptrnDlDst' <- underapprox (ptrnDlDst ptrn) (pktDlDst pkt)
    ptrnDlTyp' <- underapprox (ptrnDlTyp ptrn) (pktDlTyp pkt)
    ptrnDlVlan' <- underapprox (ptrnDlVlan ptrn) (pktDlVlan pkt)
    ptrnDlVlanPcp' <- underapprox (ptrnDlVlanPcp ptrn) (pktDlVlanPcp pkt)
    ptrnNwSrc' <- underapprox (ptrnNwSrc ptrn) (pktNwSrc pkt)
    ptrnNwDst' <- underapprox (ptrnNwDst ptrn) (pktNwDst pkt)
    ptrnNwProto' <- underapprox (ptrnNwProto ptrn) (pktNwProto pkt)
    ptrnNwTos' <- underapprox (ptrnNwTos ptrn) (pktNwTos pkt)
    ptrnTpSrc' <- underapprox (ptrnTpSrc ptrn) (pktTpSrc pkt)
    ptrnTpDst' <- underapprox (ptrnTpDst ptrn) (pktTpDst pkt)
    return $ top {
      OFMatch.srcEthAddress = fmap word48ToEth ptrnDlSrc',
      OFMatch.dstEthAddress = fmap word48ToEth ptrnDlDst',
      OFMatch.ethFrameType = ptrnDlTyp',
      OFMatch.vLANID = ptrnDlVlan',
      OFMatch.vLANPriority = ptrnDlVlanPcp',
      OFMatch.srcIPAddress = prefixToIPAddressPrefix ptrnNwSrc' ,
      OFMatch.dstIPAddress = prefixToIPAddressPrefix ptrnNwDst' ,
      OFMatch.ipProtocol = ptrnNwProto',
      OFMatch.ipTypeOfService = ptrnNwTos',
      OFMatch.srcTransportPort = ptrnTpSrc',
      OFMatch.dstTransportPort = ptrnTpDst',   
      OFMatch.inPort = ptrnInPort ptrn
      }

  toPattern ptrn = Pattern {
    ptrnDlSrc     = inverseapprox $ fmap ethToWord48 $ OFMatch.srcEthAddress ptrn,
    ptrnDlDst     = inverseapprox $ fmap ethToWord48 $ OFMatch.dstEthAddress ptrn,
    ptrnDlTyp     = inverseapprox $ OFMatch.ethFrameType ptrn,
    ptrnDlVlan    = inverseapprox $ OFMatch.vLANID ptrn,
    ptrnDlVlanPcp = inverseapprox $ OFMatch.vLANPriority ptrn,
    ptrnNwSrc     = inverseapprox $ ipAddressPrefixToPrefix $ OFMatch.srcIPAddress ptrn,
    ptrnNwDst     = inverseapprox $ ipAddressPrefixToPrefix $ OFMatch.dstIPAddress ptrn,
    ptrnNwProto   = inverseapprox $ OFMatch.ipProtocol ptrn,
    ptrnNwTos     = inverseapprox $ OFMatch.ipTypeOfService ptrn,
    ptrnTpSrc     = inverseapprox $ OFMatch.srcTransportPort ptrn,
    ptrnTpDst     = inverseapprox $ OFMatch.dstTransportPort ptrn,
    ptrnInPort    = OFMatch.inPort ptrn
    }
                   
{- Packet instance -}
                   
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

instance GPacket PacketInfo where 
  toPacket pkt = 
    Packet {
      pktInPort = receivedOnPort pkt,
      pktDlSrc = 
        ethToWord48 $ sourceMACAddress $ nettleEthernetHeaders pkt,
      pktDlDst = 
        ethToWord48 $ destMACAddress $ nettleEthernetHeaders pkt,
      pktDlTyp = 
        typeCode $ nettleEthernetHeaders pkt,
      pktDlVlan = 
        case nettleEthernetHeaders pkt of
          EthernetHeader _ _ _ -> 0xfffff 
          Ethernet8021Q _ _ _ _ _ vlan -> vlan,
      pktDlVlanPcp = 
        case nettleEthernetHeaders pkt of
          EthernetHeader _ _ _ -> 0 
          Ethernet8021Q _ _ _ pri _ _ -> pri,
      pktNwSrc = 
        stripIPAddr $ case nettleEthernetBody pkt of
          IPInEthernet (IPPacket.IPPacket hdr _) -> IPPacket.ipSrcAddress hdr
          ARPInEthernet arp -> senderIPAddress arp
          _ -> IPAddress.ipAddress 0 0 0 0,
      pktNwDst = 
        stripIPAddr $ case nettleEthernetBody pkt of
          IPInEthernet (IPPacket.IPPacket hdr _) -> IPPacket.ipDstAddress hdr
          ARPInEthernet arp -> targetIPAddress arp
          _ -> IPAddress.ipAddress 0 0 0 0,
      pktNwProto = 
        case nettleEthernetBody pkt of
          IPInEthernet (IPPacket.IPPacket hdr _) -> IPPacket.ipProtocol hdr
          ARPInEthernet arp -> 
            case arpOpCode arp of 
              ARPRequest -> 1
              ARPReply -> 2
          _ -> 0,
      pktNwTos = 
          case nettleEthernetBody pkt of
            IPInEthernet (IPPacket.IPPacket hdr _) -> IPPacket.dscp hdr
            _ -> 0 ,
      pktTpSrc = 
        case nettleEthernetBody pkt of 
          IPInEthernet (IPPacket.IPPacket _ (IPPacket.TCPInIP (src,_))) -> src
          IPInEthernet (IPPacket.IPPacket _ (IPPacket.UDPInIP (src,_))) -> src
          IPInEthernet (IPPacket.IPPacket _ (IPPacket.ICMPInIP (typ,_))) -> fromIntegral typ
          _ -> 0,
      pktTpDst =
        case nettleEthernetBody pkt of 
          IPInEthernet (IPPacket.IPPacket _ (IPPacket.TCPInIP (_,dst))) -> dst
          IPInEthernet (IPPacket.IPPacket _ (IPPacket.UDPInIP (_,dst))) -> dst
          IPInEthernet (IPPacket.IPPacket _ (IPPacket.ICMPInIP (_,cod))) -> fromIntegral cod
          _ -> 0
          }
    where
      stripIPAddr (IPAddress.IPAddress a) = a
  updatePacket = undefined

instance ValidTransmission OFMatch.Match PacketInfo where
    ptrnMatchPkt = undefined
