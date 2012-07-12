-- |Composes NetCore policies and predicates, and defines how these policies
-- interpret abstract packets.
module Frenetic.NetCore.Semantics where

import Frenetic.Compat
import Frenetic.Pattern
import Frenetic.Util
import Frenetic.NetCore.API

-- |Implements the denotation function for predicates.
interpretPredicate :: FreneticImpl a
                   => Predicate
                   -> Transmission (PatternImpl a) (PacketImpl a)
                   -> Bool
interpretPredicate (PrPattern ptrn) tr =
  FreneticPkt (toPacket (trPkt tr)) `ptrnMatchPkt` FreneticPat ptrn
interpretPredicate (PrTo sw) tr =
  sw == trSwitch tr
interpretPredicate (PrUnion pr1 pr2) tr =
  interpretPredicate pr1 tr || interpretPredicate pr2 tr
interpretPredicate (PrIntersect pr1 pr2) tr =
   interpretPredicate pr1 tr && interpretPredicate pr2 tr
interpretPredicate (PrNegate pr) tr =
  not (interpretPredicate pr tr)

-- |Implements the denotation function for policies.
interpretPolicy :: FreneticImpl a
                => Policy
                -> Transmission (PatternImpl a) (PacketImpl a)
                -> Action
interpretPolicy (PoBasic pred acts) tr = case interpretPredicate pred tr of
  True -> acts
  False -> dropPkt
interpretPolicy (PoUnion p1 p2) tr =
  interpretPolicy p1 tr `unionAction` interpretPolicy p2 tr
interpretPolicy (PoIntersect p1 p2) tr =
  interpretPolicy p1 tr `interAction` interpretPolicy p2 tr

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

  toPacket (FreneticPkt x) = x
  updatePacket pkt1 pkt2 = FreneticPkt pkt2
  ptrnMatchPkt (FreneticPkt pkt) (FreneticPat ptrn) =
    wMatch (pktDlSrc pkt) (ptrnDlSrc ptrn)
    && wMatch (pktDlDst pkt) (ptrnDlDst ptrn)
    && wMatch (pktDlTyp pkt) (ptrnDlTyp ptrn)
    && wMatch (pktDlVlan pkt) (ptrnDlVlan ptrn)
    && wMatch (pktDlVlanPcp pkt) (ptrnDlVlanPcp ptrn)
    && wMatch (pktNwSrc pkt) (ptrnNwSrc ptrn)
    && wMatch (pktNwDst pkt) (ptrnNwDst ptrn)
    && wMatch (pktNwProto pkt) (ptrnNwProto ptrn)
    && wMatch (pktNwTos pkt) (ptrnNwTos ptrn)
    && wMatch (pktTpSrc pkt) (ptrnTpSrc ptrn)
    && wMatch (pktTpDst pkt) (ptrnTpDst ptrn)
    && Just (pktInPort pkt) `match` ptrnInPort ptrn
  fromPatternOverapprox pat = FreneticPat pat
  -- We never need to underapproximate real patterns
  fromPatternUnderapprox pkt ptrn = Nothing
  toPattern (FreneticPat x) = x
  actnDefault = FreneticAct dropPkt
  actnController = FreneticAct dropPkt
  actnTranslate x = FreneticAct x
