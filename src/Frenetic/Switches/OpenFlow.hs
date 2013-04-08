module Frenetic.Switches.OpenFlow
  ( OpenFlow (..)
  , toPacket
  , actnTranslate
  , ptrnMatchPkt
  , physicalPortOfPseudoPort
  , predicateOfMatch
  ) where

import Frenetic.Common
import Data.Ord
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Frenetic.NetCore.Types as NetCore
import Data.HList
import Frenetic.NetCore.Util (modifiedFields)
import Frenetic.NetCore.Semantics
import qualified Data.Set as Set
import Data.List (find)
import qualified Nettle.IPv4.IPAddress as IPAddr
import Nettle.Ethernet.AddressResolutionProtocol
import Nettle.Servers.Server (OpenFlowServer)
import Frenetic.Pattern
import Frenetic.NetCore.Types
import Frenetic.NettleEx hiding (AllPorts)
import qualified Frenetic.NettleEx as NettleEx
import System.Log.Logger

instance Matchable IPAddressPrefix where
  top = defaultIPPrefix
  intersect = IPAddr.intersect

physicalPortOfPseudoPort (Physical p) = SendOutPort (PhysicalPort p)
physicalPortOfPseudoPort AllPorts = SendOutPort Flood
physicalPortOfPseudoPort Frenetic.NetCore.Types.InPort = SendOutPort NettleEx.InPort
physicalPortOfPseudoPort (ToQueue (Queue _ p q _)) = Enqueue p q

predicateOfMatch :: Match -> Predicate
predicateOfMatch (Match{..}) = foldl And Any (catMaybes atoms)
  where atoms = [ fmap DlSrc srcEthAddress
                , fmap DlDst dstEthAddress
                , fmap DlTyp ethFrameType
                , fmap DlVlanPcp vLANPriority
                , fmap NwProto matchIPProtocol
                , fmap NwTos ipTypeOfService
                , fmap TpSrcPort srcTransportPort
                , fmap TpSrcPort dstTransportPort
                , fmap IngressPort inPort
                ]

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

data OpenFlow = OpenFlow OpenFlowServer

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
-- TODO: unimplementable actions are basically "drop" at the moment.  When
--       the controller can handle a packet payload, then the controller
--       should be modified to resend packets sent to the controller because
--       of unimplementable actions.

-- The ToController action needs to come last. If you reorder, it will not
-- work. Observed with the usermode switch.

actnTranslate :: [Act] -> IO ActionSequence
actnTranslate acts =
  let (fwdActs, qryActs)   = List.partition isForward acts
      (noModActs, modActs) = List.partition noModifiedFields fwdActs
      actsByModSize        = List.sortBy numFieldsModified modActs
      haveMods             = length modActs > 0
      mustGetPacket        = case find isGetPacket qryActs of
                               Just _ -> True
                               Nothing -> False
      -- If there are both mod actions and a GetPacket query, or if
      -- the modifications are unimplementable, then do the actions
      -- with no modifications followed by forwarding to the controller.
      fwds   = translateFwds noModActs
      toCtrl = if mustGetPacket || cannotBeDeployed modActs
               then [SendOutPort (ToController maxBound)]
               else [] in
  do  fwdsWithMods <- if cannotBeDeployed modActs
                      then do
                          warningM "controller.switches" $ "Cannot compile actions: " ++ show acts
                          return $ []
                      else return $ translateFwds modActs
      return $ fwds ++ toCtrl ++ fwdsWithMods
  where
    -- Does the action contain modifications?
    noModifiedFields (ActFwd _ m) = Set.null $ modifiedFields m
    noModifiedFields _ = True
    -- Compare two actions based on the number of modifications they make.
    numFieldsModified (ActFwd _ m1) (ActFwd _ m2) =
      comparing Set.size (modifiedFields m1) (modifiedFields m2)
    numFieldsModified (ActFwd _ m1) _ =
      comparing Set.size (modifiedFields m1) Set.empty
    numFieldsModified _ (ActFwd _ m2) =
      comparing Set.size Set.empty (modifiedFields m2)
    numFieldsModified _ _ = comparing id 0 0
    -- Can the list of actions be deployed, or do the modifications conflict?
    cannotBeDeployed [] = False
    cannotBeDeployed fwdActs =
      let modSets = map (\(ActFwd _ m) -> modifiedFields m) fwdActs in
      not . and . map (\(m1, m2) -> m1 `Set.isSubsetOf` m2) $ adjacentPairs modSets
    -- Return a list of pairs of adjacent list elements.
    adjacentPairs []  = []
    adjacentPairs [a] = []
    adjacentPairs (a:(b:tail)) = [(a,b)] ++ adjacentPairs (b:tail)
    -- Translate a list of forward actions to OpenFlow actions.
    translateFwds fwdActs =
      concatMap
        (\(ActFwd pp md) -> modTranslate md ++ [physicalPortOfPseudoPort pp])
        fwdActs
