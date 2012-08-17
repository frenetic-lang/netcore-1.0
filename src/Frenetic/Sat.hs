module Frenetic.Sat
  ( -- * Shortcuts
    switch
  , port
  , vlan
  -- * Non-NetCore tools
  , atLoc
  , onTopo
  , transfer
  -- * Matching tools without substitution
  , match
  , forwards
  , observes
  , queries
  -- * Matching tools with substitution
  , matchWith
  , forwardsWith
  , observesWith
  , queriesWith
  ) where

import Frenetic.Z3
import Frenetic.NetCore.Types
import Frenetic.Pattern hiding (match)
import Frenetic.Topo
import Nettle.OpenFlow.Match (ofpVlanNone)
import Data.Bits
import qualified Data.Graph.Inductive.Graph as Graph
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.MultiSet as MS
import qualified Data.Set as Set

-- We use this a lot, so make a shortcut.  Need the type signature to get it to
-- be properly polymorphic.
fint :: (Integral a, Num b) => a -> b
fint = fromIntegral
switch = PktHeader "Switch"
port = PktHeader "InPort"
vlan = PktHeader "DlVlan"

atLoc :: Loc -> Z3Packet -> BoolExp
atLoc (Loc s p) pkt = ZAnd (Equals (switch pkt) (Primitive (fint s)))
                           (Equals (port pkt) (Primitive (fint p)))

-- |Build the predicate for a packet being on the topology
onTopo :: Topo -> Z3Packet -> BoolExp
onTopo topo pkt = nOr constraints where
  constraints = map onPort (Graph.labEdges topo)
  onPort (n, _, p) = atLoc (Loc (fint n) p) pkt

-- |Build the predicate for the topology transfering two packets
transfer :: Topo -> Z3Packet -> Z3Packet -> BoolExp
transfer topo p q = nAnd constraints where
  -- Keep all fields the same, but move switch and port across one of the edges
  -- in the graph.
  constraints = [ same "DlSrc"
                , same "DlDst"
                , same "DlTyp"
                , same "DlVlan"
                , same "DlVlanPcp"
                , same "NwSrc"
                , same "NwDst"
                , same "NwProto"
                , same "NwTos"
                , same "TpSrc"
                , same "TpDst"
                , forwarded]
  same field = Equals (PktHeader field p) (PktHeader field q)
  forwarded = nOr forwardOptions
  forwardOptions = map edgeToOption
                 . Set.toList
                 . Set.fromList
                 . Graph.labEdges
                 $ topo
  edgeToOption (s1, s2, port1) =
    -- safe because Topo is undirected and we start from an edge
    let Just port2 = getEdgeLabel topo s2 s1 in
    nAnd [ Equals (switch p) (Primitive (fint s1))
         , Equals (switch q) (Primitive (fint s2))
         , Equals (port p) (Primitive (fint port1))
         , Equals (port q) (Primitive (fint port2))
         ]

-- |Build the constraint for a policy forwarding an input packet to an output
-- packet.
forwards :: Policy -> Z3Packet -> Z3Packet -> BoolExp
forwards pol p q = forwardsWith pol (p, Nothing) (q, Nothing)

-- |Build the constraint for a policy forwarding an input packet to an output
-- packet with possible modifications
forwardsWith :: Policy ->
                (Z3Packet, Maybe Z3Int) ->
                (Z3Packet, Maybe Z3Int) ->
                BoolExp
forwardsWith PoBottom _ _ = ZFalse
forwardsWith (PoUnion p1 p2) p q = ZOr (forwardsWith p1 p q)
                                       (forwardsWith p2 p q)
forwardsWith (PoBasic pred action) p q = ZAnd (matchWith pred p)
                                              (produceWith action p q)

-- |Build the constraint that some query is fired by the policy.
observes :: Policy -> Z3Packet -> BoolExp
observes pol p = observesWith pol (p, Nothing)

-- |Build the constraint that some query is fired by the policy, maybe
-- substituting a value for the VLAN.
observesWith :: Policy -> (Z3Packet, Maybe Z3Int) -> BoolExp
observesWith PoBottom _ = ZFalse
observesWith (PoUnion p1 p2) p =
  if p1' == ZFalse then p2'
  else if p2' == ZFalse then p1'
  else ZOr p1' p2'
  where
    p1' = observesWith p1 p
    p2' = observesWith p2 p
observesWith (PoBasic pred action) p = if observesAct action
                                         then matchWith pred p
                                         else ZFalse

-- |Build the constraint that a particular query (as referenced by its serial
-- ID) is triggered.
queries :: Policy -> Z3Packet -> Z3Int -> BoolExp
queries pol pkt qid = queriesWith pol (pkt, Nothing) qid

-- |Build the constraint that a particular query (as referenced by its serial
-- ID) is triggered, maybe substituting a value for the VLAN.
queriesWith :: Policy -> (Z3Packet, Maybe Z3Int) -> Z3Int -> BoolExp
queriesWith PoBottom _ _ = ZFalse
queriesWith (PoUnion p1 p2) p qid =
  if p1' == ZFalse then p2'
  else if p2' == ZFalse then p1'
  else ZOr p1' p2'
  where
    p1' = queriesWith p1 p qid
    p2' = queriesWith p2 p qid
queriesWith (PoBasic pred action) p qid = if observesAct action
                                            then ZAnd (matchWith pred p)
                                                      (queryAct action qid)
                                            else ZFalse

-- |Build the constraint for pred matching packet.
match :: Predicate -> Z3Packet -> BoolExp
match pred pkt = matchWith pred (pkt, Nothing)

-- |Build the constraint for pred matching packet, maybe substituting a value
-- for the VLAN.
matchWith :: Predicate -> (Z3Packet, Maybe Z3Int) -> BoolExp
matchWith pred p@(pk, _) = case pred of
  DlSrc v -> Equals (PktHeader "DlSrc" pk) 
                    (Primitive (fromIntegral (unpackEth64 v)))
  DlDst v -> Equals (PktHeader "DlDst" pk)
                    (Primitive (fromIntegral (unpackEth64 v)))
  DlTyp v -> Equals (PktHeader "DlTyp" pk) (Primitive (fromIntegral v))
  DlVlan Nothing -> Equals (vlanOfPacket p) (Primitive ofpVlanNone)
  DlVlan (Just vlan) -> Equals (vlanOfPacket p) (Primitive (fromIntegral vlan))
  DlVlanPcp v -> Equals (PktHeader "DlVlanPcp" pk) (Primitive (fromIntegral v))
  NwSrc (Prefix v 32) -> Equals (PktHeader "NwSrc" pk)
                                (Primitive (fromIntegral v))
  NwSrc (Prefix _ _) -> error "cannot use SMT on non-exact prefixes."
  NwDst (Prefix v 32) -> Equals (PktHeader "NwDst" pk)
                                (Primitive (fromIntegral v))
  NwDst (Prefix _ _) -> error "cannot use SMT on non-exact prefixes."
  NwProto v -> Equals (PktHeader "NwProto" pk) (Primitive (fromIntegral v))
  NwTos v -> Equals (PktHeader "NwTos" pk) (Primitive (fromIntegral v))
  TpSrcPort v -> Equals (PktHeader "TpSrc" pk) (Primitive (fromIntegral v))
  TpDstPort v -> Equals (PktHeader "TpDst" pk) (Primitive (fromIntegral v))
  IngressPort v -> Equals (PktHeader "InPort" pk) (Primitive (fromIntegral v))
  Switch s -> Equals (switch pk) (Primitive (fromIntegral s))
  Or p1 p2 -> ZOr (matchWith p1 p) (matchWith p2 p)
  And p1 p2 -> ZAnd (matchWith p1 p) (matchWith p2 p)
  Not pred -> ZNot (matchWith pred p)
  Any -> ZTrue
  None -> ZFalse

-- |Build the constraint for a packet being produced from another packet by an
-- action, maybe substituting a value for VLAN on either or both
produceWith :: Action -> (Z3Packet, Maybe Z3Int) -> (Z3Packet, Maybe Z3Int)
               -> BoolExp
produceWith (Action rewrite _) p@(p', vlp) q@(q', vlq) =
  ZAnd (Equals (switch p') (switch q')) (nOr portConstraints)
  where
    portConstraints = map fieldConstraint (MS.distinctElems rewrite)
    fieldConstraint (pp, m) = nAnd cs where
      cs = (case pp of Physical port' -> [Equals (port q')
                                                 (Primitive (fint port'))]
                       -- Flood means to forward out all ports but the one we
                       -- came in
                       AllPorts  -> [ZNot (Equals (port p') (port q'))]) ++
        [ updateField p q "DlSrc"    (fmap (fint.unpackEth64) (modifyDlSrc m))
        , updateField p q "DlDst"    (fmap (fint.unpackEth64) (modifyDlDst m))
        , updateField p q "DlTyp"     Nothing
        , updateField p q "DlVlan"    (case modifyDlVlan m of
                                         Just (Just vl) -> Just (fint vl)
                                         Just Nothing -> Just ofpVlanNone
                                         Nothing -> Nothing)
        , updateField p q "DlVlanPcp" (fmap fint (modifyDlVlanPcp m))
        , updateField p q "NwSrc"     (fmap fint (modifyNwSrc m))
        , updateField p q "NwDst"     (fmap fint (modifyNwDst m))
        , updateField p q "NwProto"   Nothing
        , updateField p q "NwTos"     (fmap fint (modifyNwTos m))
        , updateField p q "TpSrc"     (fmap fint (modifyTpSrc m))
        , updateField p q "TpDst"     (fmap fint (modifyTpDst m))
        ]

-- |Determine if an action emits an observation
observesAct :: Action -> Bool
observesAct (Action _ qs) = not (MS.null qs)

-- |Determine if an action emits a particular observation
queryAct :: Action -> Z3Int -> BoolExp
queryAct (Action _ qs) qid = nOr queries where
  queries = map (\qid' -> Equals (Variable qid) (Primitive qid')) .
            map fromIntegral . map idOfQuery $
            MS.toList qs

vlanOfPacket :: (Z3Packet, Maybe Z3Int) -> IntExp
vlanOfPacket (pkt, Just vl) = Variable vl
vlanOfPacket (pkt, Nothing) = PktHeader "DlVlan" pkt

updateField :: (Z3Packet, Maybe Z3Int) ->
               (Z3Packet, Maybe Z3Int) ->
               String -> Maybe Integer ->
               BoolExp
updateField _ q "DlVlan" (Just value) =
  Equals (Primitive value) (vlanOfPacket q)
updateField p q "DlVlan" Nothing =
  Equals (vlanOfPacket p) (vlanOfPacket q)
updateField _ (q', _) field (Just value) =
  Equals (Primitive value) (PktHeader field q')
updateField (p', _) (q', _) field Nothing =
  Equals (PktHeader field p') (PktHeader field q')
