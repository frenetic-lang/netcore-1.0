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
import Frenetic.NetCore.Types
import Control.Concurrent
import Frenetic.NettleEx hiding (AllPorts, ethernetAddress64)
import qualified Frenetic.NettleEx as NettleEx

{-| Convert an EthernetAddress to a Word48. -}
ethToWord48 :: NettleEx.EthernetAddress 
            -> Frenetic.NetCore.Types.EthernetAddress
ethToWord48 = ethernetAddress64.unpack64

{-| Convert a Word48 to an EthernetAddress. -}
word48ToEth :: Frenetic.NetCore.Types.EthernetAddress
            -> NettleEx.EthernetAddress
word48ToEth = (NettleEx.ethernetAddress64).unpackEth64


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
physicalPortOfPseudoPort AllPorts = Flood

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
    Left err -> Nothing
    Right ef -> Just ef

nettleEthernetHeaders pkt = case enclosedFrame pkt of
  Right (HCons hdr _) -> Just hdr
  Left _ -> Nothing

nettleEthernetBody pkt = case enclosedFrame pkt of
  Right (HCons _ (HCons body _)) -> Just body
  Left _ -> Nothing



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

ifNothing :: Maybe a -> a -> a
ifNothing (Just a) _ = a
ifNothing Nothing b = b

modTranslate :: Modification -> ActionSequence
modTranslate (Modification{..}) =
  catMaybes [f1, f2, f3, f4, f5, f6, f7, f8, f9]
    where f1 = case modifyDlSrc of 
                 Nothing -> Nothing
                 Just v -> Just $ SetEthSrcAddr $ word48ToEth v
          f2 = case modifyDlDst of
                 Nothing -> Nothing
                 Just v -> Just $ SetEthDstAddr $ word48ToEth v
          f3 = case modifyDlVlan of
                 Nothing -> Nothing
                 Just v -> Just $ SetVlanVID v
          f4 = case modifyDlVlanPcp of
                 Nothing -> Nothing
                 Just v -> Just $ SetVlanPriority v
          f5 = case modifyNwSrc of
                Nothing -> Nothing
                Just ip -> Just $ SetIPSrcAddr $ IPAddress ip
          f6 = case modifyNwDst of
                 Nothing -> Nothing
                 Just ip -> Just $ SetIPDstAddr $ IPAddress ip
          f7 = case modifyNwTos of
                 Nothing -> Nothing
                 Just v -> Just $ SetIPToS v
          f8 = case modifyTpSrc of
                  Nothing -> Nothing
                  Just v  -> Just $ SetTransportSrcPort v
          f9 = case modifyTpDst of
                  Nothing -> Nothing
                  Just v  -> Just $ SetTransportDstPort v

instance FreneticImpl OpenFlow where
  data PacketImpl OpenFlow = OFPkt PacketInfo deriving (Show, Eq)
  data PatternImpl OpenFlow = OFPat Match deriving (Show, Eq)
  data ActionImpl OpenFlow = OFAct { fromOFAct :: ActionSequence,
                                     actQueries :: [Query] }
    deriving (Eq)

  ptrnMatchPkt (OFPkt pkt) (OFPat ptrn) = case nettleEthernetFrame pkt of
    Just frame -> matches (receivedOnPort pkt, frame) ptrn
    Nothing -> False

  toPacket (OFPkt pkt) = do
    hdrs <- nettleEthernetHeaders pkt
    body <- nettleEthernetBody pkt
    proto <- ethProto body
    tos <- ethTOS body
    return $ Packet (ethToWord48 (sourceMACAddress hdrs))
                    (ethToWord48 (destMACAddress hdrs))
                    (typeCode hdrs)
                    (ethVLANId hdrs)
                    (ethVLANPcp hdrs) 
                    (ethSrcIP body)
                    (ethDstIP body)
                    proto
                    tos
                    (srcPort body)
                    (dstPort body)
                    (receivedOnPort pkt) 

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
  actnTranslate act@(Action fwd queries) = OFAct (ofFwd ++ toCtrl) (MS.toList queries)
    where acts  = if hasUnimplementableMods $ map snd $ MS.toList fwd
                  then [SendOutPort (ToController maxBound)]
                  else ofFwd ++ toCtrl
          ofFwd = concatMap (\(pp, md) -> modTranslate md 
                                 ++ [SendOutPort (physicalPortOfPseudoPort pp)])
                      $ MS.toList fwd
          toCtrl = case find isPktQuery (MS.toList queries) of
            -- sends as much of the packet as possible to the controller
            Just _  -> [SendOutPort (ToController maxBound)]
            Nothing -> []
          hasUnimplementableMods as
            | length as <= 1 = False
            | length as > 1  =
                let minFields = foldl (\m p -> if Set.size p < Set.size m then p else m) 
                                      (modifiedFields $ head as) 
                                      (map modifiedFields $ tail as)
                in not $ all (\pat -> Set.isSubsetOf (modifiedFields pat) minFields) as
  
  actnControllerPart (OFAct _ queries) switchID ofPkt  = do
    let pktChans = map pktQueryChan . filter isPktQuery $ queries
    let sendParsablePkt chan = case toPacket ofPkt of
          Nothing -> return ()
          Just pk -> writeChan chan (switchID, pk)
    mapM_ sendParsablePkt pktChans
    
