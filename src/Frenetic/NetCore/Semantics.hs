-- |Composes NetCore policies and predicates, and defines how these policies
-- interpret abstract packets.
module Frenetic.NetCore.Semantics where

import Frenetic.Compat
import Frenetic.Pattern
import Frenetic.Common
import Frenetic.NetCore.Types
import Frenetic.NetCore.Short
import qualified Data.MultiSet as MS

-- |Implements the denotation function for predicates.
interpretPredicate :: FreneticImpl a
                   => Predicate
                   -> Transmission (PatternImpl a) (PacketImpl a)
                   -> Bool
interpretPredicate pred tr = case toPacket (trPkt tr) of
  Nothing -> False
  Just (Packet{..}) -> case pred of
    DlSrc eth -> pktDlSrc == eth
    DlDst eth -> pktDlDst == eth
    DlTyp typ -> pktDlTyp == typ
    DlVlan vlan -> pktDlVlan == vlan
    DlVlanPcp pcp -> pktDlVlanPcp == pcp
    NwSrc ipPrefix@(Prefix _ len) -> case pktNwSrc of
      Nothing -> len == 0
      Just ip -> (Prefix ip 32) `match` ipPrefix
    NwDst ipPrefix@(Prefix _ len) -> case pktNwDst of
      Nothing -> len == 0
      Just ip -> (Prefix ip 32) `match` ipPrefix 
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
interpretPolicy :: FreneticImpl a
                => Policy
                -> Transmission (PatternImpl a) (PacketImpl a)
                -> Action
interpretPolicy PoBottom tr = dropPkt
interpretPolicy (PoBasic pred acts) tr =
  if interpretPredicate pred tr then acts else dropPkt
interpretPolicy (PoUnion p1 p2) tr =
  interpretPolicy p1 tr <+> interpretPolicy p2 tr

instance Matchable (PatternImpl ()) where
  top = FreneticPat top
  intersect (FreneticPat p1) (FreneticPat p2) = case intersect p1 p2 of
    Just p3 -> Just (FreneticPat p3)
    Nothing -> Nothing
-- |
instance FreneticImpl () where
  data PacketImpl () = FreneticPkt Packet deriving (Show, Eq)
  data PatternImpl () = FreneticPat Pattern deriving (Show, Eq)
  data ActionImpl () = FreneticAct { fromFreneticAct :: Action }
    deriving (Show, Eq)

  toPacket (FreneticPkt x) = Just x

  ptrnMatchPkt (FreneticPkt pkt) (FreneticPat ptrn) =
    wMatch (pktDlSrc pkt) (ptrnDlSrc ptrn)
    && wMatch (pktDlDst pkt) (ptrnDlDst ptrn)
    && wMatch (pktDlTyp pkt) (ptrnDlTyp ptrn)
    && wMatch (pktDlVlan pkt) (ptrnDlVlan ptrn)
    && wMatch (pktDlVlanPcp pkt) (ptrnDlVlanPcp ptrn)
    && (case (pktNwSrc pkt, ptrnNwSrc ptrn) of
          (Nothing, Prefix _ len) -> len == 0
          (Just addr, prefix) -> match (Prefix addr 32) prefix)
    && (case (pktNwDst pkt, ptrnNwDst ptrn) of
          (Nothing, Prefix _ len) -> len == 0
          (Just addr, prefix) -> match (Prefix addr 32) prefix)
    && wMatch (pktNwProto pkt) (ptrnNwProto ptrn)
    && wMatch (pktNwTos pkt) (ptrnNwTos ptrn)
    && (case (pktTpSrc pkt, ptrnTpSrc ptrn) of
          (Nothing, Wildcard) -> True
          (Nothing, Exact _) -> False
          (Just pt, pat) -> wMatch pt pat)
    && (case (pktTpDst pkt, ptrnTpDst ptrn) of
          (Nothing, Wildcard) -> True
          (Nothing, Exact _) -> False
          (Just pt, pat) -> wMatch pt pat)
    && wMatch (pktInPort pkt) (ptrnInPort ptrn)
  fromPattern pat = FreneticPat pat
  actnDefault = FreneticAct dropPkt
  actnController = FreneticAct dropPkt
  actnTranslate x = FreneticAct x
  actnControllerPart (FreneticAct (Action _ queries)) switchID
                     (FreneticPkt pkt)  = do
    let pktChans = map pktQueryChan . filter isPktQuery $ MS.toList queries
    mapM_ (\chan -> writeChan chan (switchID, pkt)) pktChans
