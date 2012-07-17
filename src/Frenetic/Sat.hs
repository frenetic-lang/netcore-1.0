module Frenetic.Sat
  ( -- * Non-NetCore tools
    inSlice
  -- * Matching tools without substitution
  , match
  , forwards
  -- * Matching tools with substitution
  , matchWith
  , forwardsWith
  ) where

import Frenetic.Z3
import Frenetic.NetCore.API
import Frenetic.Pattern hiding (match)
import Frenetic.Slices.Slice
import Data.Maybe
import Data.Bits
import qualified Data.MultiSet as MS
import qualified Data.Map as Map
import qualified Data.Set as Set

-- We use this a lot, so make a shortcut.  Need the type signature to get it to
-- be properly polymorphic.
fint :: (Integral a, Num b) => a -> b
fint = fromIntegral
switch = \pkt -> PktHeader "Switch" pkt
port = \pkt -> PktHeader "InPort" pkt

-- TODO(astory): incorporate predicate checking
-- |
inSlice :: Slice -> Z3Packet -> BoolExp
inSlice (Slice int ing egr) pkt = nOr . (map atLoc) . Set.toList $ locations
  where
    locations = Set.union int (Set.union (Map.keysSet ing) (Map.keysSet egr))
    atLoc (Loc s p) = And (Equals (switch pkt) (Primitive (fint s)))
                          (Equals (port pkt) (Primitive (fint p)))

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
forwardsWith (PoUnion p1 p2) p q = Or (forwardsWith p1 p q)
                                      (forwardsWith p2 p q)
forwardsWith (PoBasic pred action) p q = And (matchWith pred p)
                                             (produceWith action p q)

-- |Build the constraint for pred matching packet.
match :: Predicate -> Z3Packet -> BoolExp
match pred pkt = matchWith pred (pkt, Nothing)

-- |Build the constraint for pred matching packet, maybe substituting a value
-- for the VLAN.
matchWith :: Predicate -> (Z3Packet, Maybe Z3Int) -> BoolExp
matchWith (PrPattern pat) p = matchPatternWith pat p
matchWith (PrTo s) (pkt, _) = Equals (switch pkt) (Primitive (fromIntegral s))
matchWith (PrUnion p1 p2) p = Or (matchWith p1 p)
                                      (matchWith p2 p)
matchWith (PrIntersect p1 p2) p = And (matchWith p1 p)
                                           (matchWith p2 p)
matchWith (PrNegate pred) p = Not (matchWith pred p)

-- |Build the constraint for a packet matching a pattern
matchPatternWith :: Pattern -> (Z3Packet, Maybe Z3Int) -> BoolExp
matchPatternWith pat p = nAnd (catMaybes matches) where
  matches = [ matchField p "DlSrc"     ((fmap fint) (ptrnDlSrc pat))
            , matchField p "DlDst"     ((fmap fint) (ptrnDlDst pat))
            , matchField p "DlTyp"     ((fmap fint) (ptrnDlTyp pat))
            , matchField p "DlVlan"    ((fmap fint) (ptrnDlVlan pat))
            , matchField p "DlVlanPcp" ((fmap fint) (ptrnDlVlanPcp pat))
            , matchField p "NwSrc"     (fromPrefix  (ptrnNwSrc pat))
            , matchField p "NwDst"     (fromPrefix  (ptrnNwDst pat))
            , matchField p "NwProto"   ((fmap fint) (ptrnNwProto pat))
            , matchField p "NwTos"     ((fmap fint) (ptrnNwTos pat))
            , matchField p "TpSrc"     ((fmap fint) (ptrnTpSrc pat))
            , matchField p "TpDst"     ((fmap fint) (ptrnTpDst pat))
            , matchField p "InPort"    ((fmap fint) (ptrnInPort pat))
            ]

fromPrefix :: (Integral a, Bits a) => Prefix a -> Wildcard Integer
fromPrefix (Prefix value 0) = Wildcard
fromPrefix (Prefix value v) =
  if v == bitSize value then Exact (fromIntegral value)
                        else error "Cannot use SMT on non-exact prefixes."

matchField :: (Z3Packet, Maybe Z3Int) ->
              String -> Wildcard Integer ->
              Maybe BoolExp
matchField pkt "DlVlan" (Exact value) =
  Just (Equals (vlanOfPacket pkt) (Primitive value))
matchField (pkt', _) field (Exact value) =
  Just (Equals (PktHeader field pkt') (Primitive value))
matchField _ _ Wildcard = Nothing

-- |Build the constraint for a packet being produced from another packet by an
-- action, maybe substituting a value for VLAN on either or both
produceWith :: Action -> (Z3Packet, Maybe Z3Int) -> (Z3Packet, Maybe Z3Int)
               -> BoolExp
produceWith (Action rewrite _) p@(p', vlp) q@(q', vlq) =
  And (Equals (switch p') (switch q')) (nOr portConstraints)
  where
    portConstraints = map fieldConstraint (MS.distinctElems rewrite)
    fieldConstraint (pp, m) = nAnd cs where
      cs = (case pp of Physical port' -> [Equals (port q')
                                                 (Primitive (fint port'))]
                       PhysicalFlood  -> []) ++
        [ updateField p q "DlSrc"     ((fmap fint) (ptrnDlSrc m))
        , updateField p q "DlDst"     ((fmap fint) (ptrnDlDst m))
        , updateField p q "DlTyp"     ((fmap fint) (ptrnDlTyp m))
        , updateField p q "DlVlan"    ((fmap fint) (ptrnDlVlan m))
        , updateField p q "DlVlanPcp" ((fmap fint) (ptrnDlVlanPcp m))
        , updateField p q "NwSrc"     (fromPrefix  (ptrnNwSrc m))
        , updateField p q "NwDst"     (fromPrefix  (ptrnNwDst m))
        , updateField p q "NwProto"   ((fmap fint) (ptrnNwProto m))
        , updateField p q "NwTos"     ((fmap fint) (ptrnNwTos m))
        , updateField p q "TpSrc"     ((fmap fint) (ptrnTpSrc m))
        , updateField p q "TpDst"     ((fmap fint) (ptrnTpDst m))
        ]

vlanOfPacket :: (Z3Packet, Maybe Z3Int) -> IntExp
vlanOfPacket (pkt, Just vl) = Variable vl
vlanOfPacket (pkt, Nothing) = PktHeader "DlVlan" pkt

updateField :: (Z3Packet, Maybe Z3Int) ->
               (Z3Packet, Maybe Z3Int) ->
               String -> Wildcard Integer ->
               BoolExp
updateField _ q "DlVlan" (Exact value) =
  Equals (Primitive value) (vlanOfPacket q)
updateField p q "DlVlan" (Wildcard) =
  Equals (vlanOfPacket p) (vlanOfPacket q)
updateField _ (q', _) field (Exact value) =
  Equals (Primitive value) (PktHeader field q')
updateField (p', _) (q', _) field Wildcard =
  Equals (PktHeader field p') (PktHeader field q')
