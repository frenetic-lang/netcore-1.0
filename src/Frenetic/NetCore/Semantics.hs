-- |Composes NetCore policies and predicates, and defines how these policies
-- interpret abstract packets.
module Frenetic.NetCore.Semantics where

import Nettle.OpenFlow.Match
import Nettle.IPv4.IPAddress
import Frenetic.Pattern
import Frenetic.Common
import Frenetic.NetCore.Types
import Frenetic.NetCore.Short
import Frenetic.Switches.OpenFlow
import qualified Data.Map as M

evalProgram :: Program
            -> (Map Switch [Queue], Policy)
evalProgram prog = (M.fromListWith (++) queues, pol)
  where (qMap, pol) = eval M.empty prog 
        queues = concatMap (\((sw, _), (_, qs)) -> zip [sw ..] (map (:[]) qs))
                           -- I assume nobody is reading this code.
                           (M.toList qMap)
        eval qMap prog = case prog of
          Policy pol -> (qMap, pol)
          ProgramUnion prog1 prog2 ->
            let (qMap', pol1) = eval qMap prog1
                (qMap'', pol2) = eval qMap' prog2
              in (qMap'', pol1 `mappend` pol2)
          WithQueue sw pt rate fn -> case M.lookup (sw,pt) qMap of
            Nothing -> 
              let q = Queue sw pt 1 rate
                in eval (M.insert (sw,pt) (1, [q]) qMap) (fn q)
            Just (n, qs) ->
              let q = Queue sw pt (n+1) rate
                in eval (M.insert (sw,pt) (n+1, q : qs) qMap) (fn q)
          

-- |Implements the denotation function for predicates.
interpretPredicate :: Predicate
                   -> LocPacket
                   -> Bool
interpretPredicate pred lp@(Loc switch port, Packet{..}) = case pred of
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
  IngressPort pt -> pt == port
  Any -> True
  None -> False
  Switch sw -> sw == switch
  Or pr1 pr2 -> interpretPredicate pr1 lp || interpretPredicate pr2 lp
  And pr1 pr2 -> interpretPredicate pr1 lp && interpretPredicate pr2 lp
  Not pr -> not (interpretPredicate pr lp)

-- |Implements the denotation function for policies.
interpretPolicy :: Policy
                -> LocPacket
                -> [Action]
interpretPolicy PoBottom lp = dropPkt
interpretPolicy (PoBasic pred acts) lp =
  if interpretPredicate pred lp then acts else dropPkt
interpretPolicy (PoUnion p1 p2) lp =
  interpretPolicy p1 lp <+> interpretPolicy p2 lp
interpretPolicy (Restrict pol pred) lp =
  if interpretPredicate pred lp then
    interpretPolicy pol lp
  else
    []
interpretPolicy (SendPackets chan) _ =
  []
