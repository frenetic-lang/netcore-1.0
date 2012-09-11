module Frenetic.Switches.OpenFlow
  ( OpenFlow (..)
  , toOFAct
  , Nettle (..)
  , ActionImpl (..)
  , toPacket
  , actnTranslate
  , ptrnMatchPkt
  , actnControllerPart
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Frenetic.NetCore.Types as NetCore
import Data.HList
import Frenetic.NetCore.Util
import Control.Concurrent.Chan
import           Data.Bits
import qualified Data.Set                        as Set
import           Data.Word
import Data.List (nub, find)
import qualified Nettle.IPv4.IPAddress as IPAddr
import Nettle.Ethernet.AddressResolutionProtocol
import Frenetic.Pattern
import Frenetic.NetCore.Types
import Control.Concurrent
import Frenetic.NettleEx hiding (AllPorts)
import qualified Frenetic.NettleEx as NettleEx

instance Matchable IPAddressPrefix where
  top = defaultIPPrefix
  intersect = IPAddr.intersect

physicalPortOfPseudoPort (Physical p) = PhysicalPort p
physicalPortOfPseudoPort AllPorts = Flood

toController :: ActionSequence
toController = sendToController maxBound

instance Eq a => Matchable (Maybe a) where
  top = Nothing
  intersect (Just a) (Just b) = if a == b then Just (Just a) else Nothing
  intersect (Just a) Nothing = Just (Just a)
  intersect Nothing (Just b) = Just (Just b)
  intersect Nothing Nothing  = Just Nothing

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

toOFAct :: ActionSequence -> ActionImpl
toOFAct p = OFAct p []

instance Show ActionImpl where
  show (OFAct acts ls) = show acts ++ " and " ++ show (length ls) ++ " queries"

ifNothing :: Maybe a -> a -> a
ifNothing (Just a) _ = a
ifNothing Nothing b = b

modTranslate :: Modification -> ActionSequence
modTranslate (Modification{..}) =
  catMaybes [f1, f2, f3, f4, f5, f6, f7, f8, f9]
    where f1 = case modifyDlSrc of
                 Nothing -> Nothing
                 Just v -> Just $ SetEthSrcAddr  v
          f2 = case modifyDlDst of
                 Nothing -> Nothing
                 Just v -> Just $ SetEthDstAddr v
          f3 = case modifyDlVlan of
                 Nothing -> Nothing
                 Just (Just v) -> Just $ SetVlanVID v
                 Just Nothing -> Just StripVlanHeader
          f4 = case modifyDlVlanPcp of
                 Nothing -> Nothing
                 Just v -> Just $ SetVlanPriority v
          f5 = case modifyNwSrc of
                Nothing -> Nothing
                Just ip -> Just $ SetIPSrcAddr ip
          f6 = case modifyNwDst of
                 Nothing -> Nothing
                 Just ip -> Just $ SetIPDstAddr ip
          f7 = case modifyNwTos of
                 Nothing -> Nothing
                 Just v -> Just $ SetIPToS v
          f8 = case modifyTpSrc of
                  Nothing -> Nothing
                  Just v  -> Just $ SetTransportSrcPort v
          f9 = case modifyTpDst of
                  Nothing -> Nothing
                  Just v  -> Just $ SetTransportDstPort v

data ActionImpl  = OFAct { fromOFAct :: ActionSequence,
                         actQueries :: [NetCore.Action] }
                         deriving (Eq)

ptrnMatchPkt pkt ptrn = case nettleEthernetFrame pkt of
  Just frame -> matches (receivedOnPort pkt, frame) ptrn
  Nothing -> False

toPacket pkt = do
  hdrs <- nettleEthernetHeaders pkt
  body <- nettleEthernetBody pkt
  proto <- ethProto body
  tos <- ethTOS body
  return $ Packet (sourceMACAddress hdrs)
                  (destMACAddress hdrs)
                  (typeCode hdrs)
                  (ethVLANId hdrs)
                  (ethVLANPcp hdrs)
                  (fmap IPAddress (ethSrcIP body))
                  (fmap IPAddress (ethDstIP body))
                  proto
                  tos
                  (srcPort body)
                  (dstPort body)
                  (receivedOnPort pkt)


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
actnTranslate act = OFAct (ofFwd ++ toCtrl) queries
  where acts  = if hasUnimplementableMods $ map (\(Forward _ m) -> m) fwd
                then [SendOutPort (ToController maxBound)]
                else ofFwd ++ toCtrl
        ofFwd = concatMap (\(Forward pp md) -> modTranslate md
                               ++ [SendOutPort (physicalPortOfPseudoPort pp)])
                    fwd
        toCtrl = case find isGetPacket queries of
          -- sends as much of the packet as possible to the controller
          Just _  -> [SendOutPort (ToController maxBound)]
          Nothing -> []
        fwd = filter isForward act
        queries = filter isQuery act
        hasUnimplementableMods as
          | length as <= 1 = False
          | otherwise =
              let minFields = foldl (\m p -> if Set.size p < Set.size m then p else m)
                                    (modifiedFields $ head as)
                                    (map modifiedFields $ tail as)
              in not $ all (\pat -> Set.isSubsetOf (modifiedFields pat) minFields) as

actnControllerPart (OFAct _ queries) switchID ofPkt  = do
  let pktActions = map getPacketAction . filter isGetPacket $ queries
  let sendParsablePkt act = case toPacket ofPkt of
        Nothing -> return ()
        Just pk -> act (switchID, pk)
  mapM_ sendParsablePkt pktActions

