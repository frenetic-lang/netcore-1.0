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
-- - Redistributions of source code must retain the above copyright           --
--   notice, this list of conditions and the following disclaimer.            --
-- - Redistributions of binaries must reproduce the above copyright           --
--   notice, this list of conditions and the following disclaimer in          --
--   the documentation or other materials provided with the distribution.     --
-- - The names of the copyright holds and contributors may not be used to     --
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
--------------------------------------------------------------------------------

module Frenetic.Switches.OpenFlow
  ( prefixToIPAddressPrefix
  , ipAddressPrefixToPrefix
  , OpenFlow (..)
  , toOFPkt
  , fromOFPkt
  , toOFPat
  , fromOFPat
  , toOFAct
  , fromOFAct
  , Nettle (..)
  , actQueries
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)

import Data.HList
import Control.Concurrent.Chan
import           Data.Bits
import           Frenetic.LargeWord
import qualified Data.Set                        as Set
import qualified Data.MultiSet                   as MS
import           Data.Word
import Data.List (nub, find)
import qualified Nettle.IPv4.IPAddress as IPAddr
import Nettle.Ethernet.AddressResolutionProtocol
import Frenetic.Pattern
import Frenetic.Compat
import Frenetic.NetCore.API
import Control.Concurrent
import Frenetic.NettleEx

{-| Convert an EthernetAddress to a Word48. -}
ethToWord48 eth =
  LargeKey a (LargeKey b (LargeKey c (LargeKey d (LargeKey e f))))
     where (f, e, d, c, b, a) = unpack eth

{-| Convert a Word48 to an EthernetAddress. -}
word48ToEth (LargeKey a (LargeKey b (LargeKey c (LargeKey d (LargeKey e f))))) =
    ethernetAddress f e d c b a

{-| Convert a pattern Prefix to an IPAddressPrefix. -}
prefixToIPAddressPrefix :: Prefix Word32 -> IPAddressPrefix
prefixToIPAddressPrefix (Prefix x len) = (IPAddress x, fromIntegral len)

{-| Convert an IPAddressPrefix to a pattern Prefix. -}
ipAddressPrefixToPrefix :: IPAddressPrefix -> Prefix Word32
ipAddressPrefixToPrefix (IPAddress x, len) = Prefix x (fromIntegral len)

instance Matchable IPAddressPrefix where
  top = defaultIPPrefix
  intersect = IPAddr.intersect

physicalPortOfPseudoPort (Physical p) = PhysicalPort p
physicalPortOfPseudoPort PhysicalFlood = Flood

toController :: ActionSequence
toController = sendToController maxBound

instance Eq a => Matchable (Maybe a) where
  top = Nothing
  intersect (Just a) (Just b) = case a == b of
    True  -> Just (Just a)
    False -> Nothing
  intersect (Just a) Nothing = Just (Just a)
  intersect Nothing (Just b) = Just (Just b)
  intersect Nothing Nothing  = Just Nothing

wildcardToMaybe (Exact a) = Just a
wildcardToMaybe Wildcard  = Nothing

maybeToWildcard (Just a) = Exact a
maybeToWildcard Nothing  = Wildcard

instance Matchable Match where
  top = Match {
          inPort = Nothing,
          srcEthAddress = top,
          dstEthAddress = top,
          vLANID = top,
          vLANPriority = top,
          ethFrameType = top,
          ipTypeOfService = top,
          matchIPProtocol = top,
          srcIPAddress = top,
          dstIPAddress = top,
          srcTransportPort = top,
          dstTransportPort = top }

  intersect ofm1 ofm2 =
      do inport <- intersect (inPort ofm1) (inPort ofm2)
         srcethaddress <- intersect (srcEthAddress ofm1) (srcEthAddress ofm2)
         dstethaddress <- intersect (dstEthAddress ofm1) (dstEthAddress ofm2)
         vlanid <- intersect (vLANID ofm1) (vLANID ofm2)
         vlanpriority <- intersect (vLANPriority ofm1) (vLANPriority ofm2)
         ethframetype <- intersect (ethFrameType ofm1) (ethFrameType ofm2)
         iptypeofservice <- intersect (ipTypeOfService ofm1) (ipTypeOfService ofm2)
         ipprotocol <- intersect (matchIPProtocol ofm1) (matchIPProtocol ofm2)
         srcipaddress <- intersect (srcIPAddress ofm1) (srcIPAddress ofm2)
         dstipaddress <- intersect (dstIPAddress ofm1) (dstIPAddress ofm2)
         srctransportport <- intersect (srcTransportPort ofm1) (srcTransportPort ofm2)
         dsttransportport <- intersect (dstTransportPort ofm1) (dstTransportPort ofm2)
         return Match {
           inPort = inport,
           srcEthAddress = srcethaddress,
           dstEthAddress = dstethaddress,
           vLANID = vlanid,
           vLANPriority = vlanpriority,
           ethFrameType = ethframetype,
           ipTypeOfService = iptypeofservice,
           matchIPProtocol = ipprotocol,
           srcIPAddress = srcipaddress,
           dstIPAddress = dstipaddress,
           srcTransportPort = srctransportport,
           dstTransportPort = dsttransportport }


nettleEthernetFrame pkt = case enclosedFrame pkt of
    Left err -> error ("Expected an Ethernet frame: " ++ err)
    Right ef -> ef

nettleEthernetHeaders pkt = case enclosedFrame pkt of
  Right (HCons hdr _) -> hdr
  Left _ -> error "no ethernet headers"

nettleEthernetBody pkt = case enclosedFrame pkt of
  Right (HCons _ (HCons body _)) -> body
  Left _ -> error "no ethernet body"

data OpenFlow = OpenFlow Nettle

instance Matchable (PatternImpl OpenFlow) where
  top = OFPat top
  intersect (OFPat p1) (OFPat p2) = case Frenetic.Pattern.intersect p1 p2 of
    Just p3 -> Just (OFPat p3)
    Nothing -> Nothing

toOFPkt :: PacketInfo -> PacketImpl OpenFlow
toOFPkt p = OFPkt p

fromOFPkt :: PacketImpl OpenFlow -> PacketInfo
fromOFPkt (OFPkt p) = p

toOFPat :: Match -> PatternImpl OpenFlow
toOFPat p = OFPat p

fromOFPat :: PatternImpl OpenFlow -> Match
fromOFPat (OFPat p) = p

toOFAct :: ActionSequence -> ActionImpl OpenFlow
toOFAct p = OFAct p []

instance Show (ActionImpl OpenFlow) where
  show (OFAct acts ls) = show acts ++ " and " ++ show (length ls) ++ " queries"

instance FreneticImpl OpenFlow where
  data PacketImpl OpenFlow = OFPkt PacketInfo deriving (Show, Eq)
  data PatternImpl OpenFlow = OFPat Match deriving (Show, Eq)
  data ActionImpl OpenFlow = OFAct { fromOFAct :: ActionSequence,
                                     actQueries :: [Query] }
    deriving (Eq)

  ptrnMatchPkt (OFPkt pkt) (OFPat ptrn) =
    matches (receivedOnPort pkt, nettleEthernetFrame pkt) ptrn

  toPacket (OFPkt pkt) = Packet {
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
          IPInEthernet (HCons hdr _) -> ipSrcAddress hdr
          ARPInEthernet (ARPQuery q) -> querySenderIPAddress q
          ARPInEthernet (ARPReply r) -> replySenderIPAddress r
          _ -> ipAddress 0 0 0 0,
      pktNwDst =
        stripIPAddr $ case nettleEthernetBody pkt of
          IPInEthernet (HCons hdr _) -> ipDstAddress hdr
          ARPInEthernet (ARPQuery q) -> queryTargetIPAddress q
          ARPInEthernet (ARPReply r) -> replyTargetIPAddress r
          _ -> ipAddress 0 0 0 0,
      pktNwProto =
        case nettleEthernetBody pkt of
          IPInEthernet (HCons hdr _) -> ipProtocol hdr
          ARPInEthernet (ARPQuery _) -> 1
          ARPInEthernet (ARPReply _) -> 2
          _ -> 0,
      pktNwTos =
          case nettleEthernetBody pkt of
            IPInEthernet (HCons hdr _) -> dscp hdr
            _ -> 0 ,
      pktTpSrc =
        case nettleEthernetBody pkt of
          IPInEthernet (HCons _ (HCons (TCPInIP (src,dst)) _)) -> src
          IPInEthernet (HCons _ (HCons (UDPInIP (src,dst) _) _)) -> src
          IPInEthernet (HCons _ (HCons (ICMPInIP (typ,cod)) _)) ->
            fromIntegral typ
          _ -> 0,
      pktTpDst =
        case nettleEthernetBody pkt of
          IPInEthernet (HCons _ (HCons (TCPInIP (src,dst)) _)) -> dst
          IPInEthernet (HCons _ (HCons (UDPInIP (src,dst) _) _)) -> dst
          IPInEthernet (HCons _ (HCons (ICMPInIP (typ,cod)) _)) ->
            fromIntegral cod
          _ -> 0
          }
    where
      stripIPAddr (IPAddress a) = a

  fromPattern ptrn = OFPat $ Match {
    srcEthAddress = wildcardToMaybe $ fmap word48ToEth (ptrnDlSrc ptrn),
    dstEthAddress = wildcardToMaybe $ fmap word48ToEth (ptrnDlDst ptrn),
    ethFrameType = wildcardToMaybe $ ptrnDlTyp ptrn,
    vLANID = wildcardToMaybe $ ptrnDlVlan ptrn,
    vLANPriority = wildcardToMaybe $ ptrnDlVlanPcp ptrn,
    srcIPAddress = prefixToIPAddressPrefix (ptrnNwSrc ptrn),
    dstIPAddress = prefixToIPAddressPrefix (ptrnNwDst ptrn),
    matchIPProtocol = wildcardToMaybe $ ptrnNwProto ptrn,
    ipTypeOfService = wildcardToMaybe $ ptrnNwTos ptrn,
    srcTransportPort = wildcardToMaybe $ ptrnTpSrc ptrn,
    dstTransportPort = wildcardToMaybe $ ptrnTpDst ptrn,
    inPort = wildcardToMaybe $ ptrnInPort ptrn
  }

  toPattern (OFPat ptrn) = Pattern {
    ptrnDlSrc     = maybeToWildcard $ fmap ethToWord48 $ srcEthAddress ptrn,
    ptrnDlDst     = maybeToWildcard $ fmap ethToWord48 $ dstEthAddress ptrn,
    ptrnDlTyp     = maybeToWildcard $ ethFrameType ptrn,
    ptrnDlVlan    = maybeToWildcard $ vLANID ptrn,
    ptrnDlVlanPcp = maybeToWildcard $ vLANPriority ptrn,
    ptrnNwSrc     = ipAddressPrefixToPrefix $ srcIPAddress ptrn,
    ptrnNwDst     = ipAddressPrefixToPrefix $ dstIPAddress ptrn,
    ptrnNwProto   = maybeToWildcard $ matchIPProtocol ptrn,
    ptrnNwTos     = maybeToWildcard $ ipTypeOfService ptrn,
    ptrnTpSrc     = maybeToWildcard $ srcTransportPort ptrn,
    ptrnTpDst     = maybeToWildcard $ dstTransportPort ptrn,
    ptrnInPort    = maybeToWildcard $ inPort ptrn
    }

  actnController = OFAct toController []
  actnDefault = OFAct toController []

  -- Not all multisets of actions can be translated to lists of OpenFlow
  -- actions.  For example, consider the set 
  --
  --    {(SrcIP = 0, Fwd 1), (DstIP = 1, Fwd 2)}.
  --
  -- In general, it is not possible to set SrcIP to 0, forward out port 1,
  -- and then revert the SrcIP to its original value before setting DstIP to 1
  -- and forwarding out port 2.  In such situations, the action is changed to
  -- "send to controller."
  -- TODO: implement optimizations to handle special cases on the switch.
  -- TODO: deploying these kinds of actions relies on the controller to forward
  --       them.  will this corrupt/drop packets larger than maxBound?

  -- The ToController action needs to come last. If you reorder, it will not
  -- work. Observed with the usermode switch.
  actnTranslate act@(Action fwd queries) = OFAct (ofFwd ++ toCtrl) queries
    where acts  = if hasUnimplementableMods $ map snd $ MS.toList fwd
                  then [SendOutPort (ToController maxBound)]
                  else ofFwd ++ toCtrl
          ofFwd = concatMap (\(pp, md) -> modTranslate md 
                                 ++ [SendOutPort (physicalPortOfPseudoPort pp)])
                      $ MS.toList fwd
          toCtrl = case find isPktQuery queries of
            -- sends as much of the packet as possible to the controller
            Just _  -> [SendOutPort (ToController maxBound)]
            Nothing -> []
          hasUnimplementableMods as
            | length as <= 1 = False
            | length as > 1  =
                let minFields = foldl (\m p -> if Set.size p < Set.size m then p else m) 
                                      (interestingFields $ head as) 
                                      (map interestingFields $ tail as)
                in not $ all (\pat -> Set.isSubsetOf (interestingFields pat) minFields) as
          modTranslate m = 
            let f1 = case ptrnDlSrc m of 
                       Wildcard -> Nothing
                       Exact v  -> Just $ SetEthSrcAddr $ word48ToEth v
                f2 = case ptrnDlDst m of
                       Wildcard -> Nothing
                       Exact v  -> Just $ SetEthDstAddr $ word48ToEth v
                f3 = case ptrnDlTyp m of
                       Wildcard -> Nothing
                       Exact v  -> error "OpenFlow 1.0 does not support modifying DlTyp."
                f4 = case ptrnDlVlan m of
                       Wildcard -> Nothing
                       Exact v  -> Just $ SetVlanVID v
                f5 = case ptrnDlVlanPcp m of
                       Wildcard -> Nothing
                       Exact v  -> Just $ SetVlanPriority v
                f6 = case ptrnNwSrc m of
                       Prefix _  0  -> Nothing
                       Prefix ip 32 -> Just $ SetIPSrcAddr $ IPAddress ip
                       _ -> error "IP Modification must specify the entire IP address."
                f7 = case ptrnNwDst m of
                       Prefix _  0  -> Nothing
                       Prefix ip 32 -> Just $ SetIPDstAddr $ IPAddress ip
                       _ -> error "IP Modification must specify the entire IP address."
                f8 = case ptrnNwProto m of
                       Wildcard -> Nothing
                       Exact v  -> error "OpenFlow 1.0 does not support modifying NwProto."
                f9 = case ptrnNwTos m of
                       Wildcard -> Nothing
                       Exact v  -> Just $ SetIPToS v
                f10 = case ptrnTpSrc m of
                        Wildcard -> Nothing
                        Exact v  -> Just $ SetTransportSrcPort v
                f11 = case ptrnTpDst m of
                        Wildcard -> Nothing
                        Exact v  -> Just $ SetTransportDstPort v
              in catMaybes [f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11]
  
  actnControllerPart (OFAct _ queries) switchID ofPkt  = do
    let pktChans = map pktQueryChan . filter isPktQuery $ queries
    let pkt = toPacket ofPkt
    mapM_ (\chan -> writeChan chan (switchID, pkt)) pktChans
    
