-- |Composes NetCore policies and predicates, and defines how these policies
-- interpret abstract packets.
module Frenetic.NetCore.Semantics where

import Frenetic.Compat
import Frenetic.Pattern
import Frenetic.Util

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
  actnDefault = FreneticAct emptyAction
  actnController = FreneticAct emptyAction
  actnTranslate x = FreneticAct x

-- |Needed for Matchable (PatternImpl ())
instance Matchable Pattern where
  top = Pattern {
    ptrnDlSrc = top
    , ptrnDlDst = top
    , ptrnDlTyp = top
    , ptrnDlVlan = top
    , ptrnDlVlanPcp = top
    , ptrnNwSrc = top
    , ptrnNwDst = top
    , ptrnNwProto = top
    , ptrnNwTos = top
    , ptrnTpSrc = top
    , ptrnTpDst = top
    , ptrnInPort = top
    }
        
  intersect p1 p2 = do ptrnDlSrc' <- intersect (ptrnDlSrc p1) (ptrnDlSrc p2)
                       ptrnDlDst' <- intersect (ptrnDlDst p1) (ptrnDlDst p2)
                       ptrnDlTyp' <- intersect (ptrnDlTyp p1) (ptrnDlTyp p2)
                       ptrnDlVlan' <- intersect (ptrnDlVlan p1) (ptrnDlVlan p2)
                       ptrnDlVlanPcp' <- intersect (ptrnDlVlanPcp p1) (ptrnDlVlanPcp p2)
                       ptrnNwSrc' <- intersect (ptrnNwSrc p1) (ptrnNwSrc p2)
                       ptrnNwDst' <- intersect (ptrnNwDst p1) (ptrnNwDst p2)
                       ptrnNwProto' <- intersect (ptrnNwProto p1) (ptrnNwProto p2)
                       ptrnNwTos' <- intersect (ptrnNwTos p1) (ptrnNwTos p2)
                       ptrnTpSrc' <- intersect (ptrnTpSrc p1) (ptrnTpSrc p2)
                       ptrnTpDst' <- intersect (ptrnTpDst p1) (ptrnTpDst p2)
                       ptrnInPort' <- intersect (ptrnInPort p1) (ptrnInPort p2)
                       return Pattern {
                         ptrnDlSrc = ptrnDlSrc'
                         , ptrnDlDst = ptrnDlDst'
                         , ptrnDlTyp = ptrnDlTyp'
                         , ptrnDlVlan = ptrnDlVlan'
                         , ptrnDlVlanPcp = ptrnDlVlanPcp'
                         , ptrnNwSrc = ptrnNwSrc'
                         , ptrnNwDst = ptrnNwDst'
                         , ptrnNwProto = ptrnNwProto'
                         , ptrnNwTos = ptrnNwTos'
                         , ptrnTpSrc = ptrnTpSrc'
                         , ptrnTpDst = ptrnTpDst'
                         , ptrnInPort = ptrnInPort'
                         }