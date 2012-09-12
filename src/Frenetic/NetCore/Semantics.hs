-- |Composes NetCore policies and predicates, and defines how these policies
-- interpret abstract packets.
module Frenetic.NetCore.Semantics where

import Nettle.OpenFlow.Match
import Nettle.IPv4.IPAddress
import Frenetic.Compat
import Frenetic.Pattern
import Frenetic.Common
import Frenetic.NetCore.Types
import Frenetic.NetCore.Short
import Frenetic.Switches.OpenFlow

-- |Implements the denotation function for predicates.
interpretPredicate :: Predicate
                   -> Transmission Match Packet
                   -> Bool
interpretPredicate pred tr@(Transmission _ sw Packet{..}) = case pred of
  DlSrc eth -> pktDlSrc == eth
  DlDst eth -> pktDlDst == eth
  DlTyp typ -> pktDlTyp == typ
  DlVlan vlan -> pktDlVlan == vlan
  DlVlanPcp pcp -> pktDlVlanPcp == pcp
  NwSrc ipPrefix@(IPAddressPrefix _ len) -> case pktNwSrc of
    Nothing -> len == 0
    Just ip -> ip `elemOfPrefix` ipPrefix
  NwDst ipPrefix@(IPAddressPrefix _ len) -> case pktNwDst of
    Nothing -> len == 0
    Just ip -> ip `elemOfPrefix` ipPrefix
  NwProto proto -> pktNwProto == proto
  NwTos tos -> pktNwTos == tos
  TpSrcPort pt -> case pktTpSrc of
    Nothing -> False
    Just pt' -> pt == pt'
  TpDstPort pt -> case pktTpDst of
    Nothing -> False
    Just pt' -> pt == pt'
  IngressPort pt -> pt == pktInPort
  Any -> True
  None -> False
  Switch sw -> sw == trSwitch tr
  Or pr1 pr2 -> interpretPredicate pr1 tr || interpretPredicate pr2 tr
  And pr1 pr2 -> interpretPredicate pr1 tr && interpretPredicate pr2 tr
  Not pr -> not (interpretPredicate pr tr)

-- |Implements the denotation function for policies.
interpretPolicy :: Policy
                -> Transmission Match Packet
                -> [Action]
interpretPolicy PoBottom tr = dropPkt
interpretPolicy (PoBasic pred acts) tr =
  if interpretPredicate pred tr then acts else dropPkt
interpretPolicy (PoUnion p1 p2) tr =
  interpretPolicy p1 tr <+> interpretPolicy p2 tr
interpretPolicy (Restrict pol pred) tr =
  if interpretPredicate pred tr then
    interpretPolicy pol tr
  else
    []
interpretPolicy (SendPackets chan) _ =
  []

