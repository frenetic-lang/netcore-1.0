module Frenetic.NetCore.Short
  ( -- * Shorthand constructors
  -- ** Predicates
    inport
  , (<||>)
  , (<&&>)
  , matchAll
  , matchNone
  , neg
  , prSubtract
  , prOr
  , prAnd
  -- ** Actions
  , dropPkt
  , allPorts
  , forward
  , modify
  -- ** Policies
  , (==>)
  , (<%>)
  , (<+>)
  -- * Exact match predicate constructors
  , onSwitch
  , dlSrc
  , dlDst
  , dlTyp
  , dlVlan
  , dlVlanPcp
  , nwSrc
  , nwDst
  , nwSrcPrefix
  , nwDstPrefix
  , nwProto
  , nwTos
  , tpSrc
  , tpDst
  , inPort
  -- * Packet modifications
  , Modification (..)
  , unmodified
  , modDlSrc
  , modDlDst
  , modDlVlan
  , modDlVlanPcp
  , modNwSrc
  , modNwDst
  , modNwTos
  , modTpSrc
  , modTpDst
  ) where

import Data.Word
import qualified Data.List as List
import qualified Data.MultiSet as MS
import Frenetic.Pattern
import Frenetic.NetCore.Types
import Data.Monoid

-- |Matches all packets.
matchAll :: Predicate
matchAll = top

-- |Matches no packets.
matchNone :: Predicate
matchNone = PrNegate top

-- |Construct the predicate matching packets on this switch and port
inport :: Switch -> Port -> Predicate
inport switch port = PrIntersect (PrTo switch)
                                 (PrPattern (top {ptrnInPort = Exact port}))

-- |Construct the set difference between p1 and p2
prSubtract :: Predicate -> Predicate -> Predicate
prSubtract p1 p2 = PrIntersect p1 (PrNegate p2)

-- |Construct nary union of a list of predicates
prOr :: [Predicate] -> Predicate
prOr [] = neg top
prOr ps = List.foldr1 (\ p1 p2 -> PrUnion p1 p2) ps

-- |Construct the intersect of a list of predicates
prAnd :: [Predicate] -> Predicate
prAnd [] = top
prAnd ps = List.foldr1 (\ p1 p2 -> PrIntersect p1 p2) ps

dropPkt :: Action
dropPkt = Action MS.empty MS.empty

allPorts :: Modification -> Action
allPorts mod = Action (MS.singleton (AllPorts, mod)) MS.empty

forward :: [Port] -> Action
forward ports = Action (MS.fromList lst) MS.empty
  where lst = [ (Physical p, unmodified) | p <- ports ]

modify :: [(Port, Modification)] -> Action
modify mods = Action (MS.fromList lst) MS.empty
  where lst = [ (Physical p, mod) | (p, mod) <- mods ]

onSwitch = PrTo

instance Monoid Action where
  mappend (Action fwd1 q1) (Action fwd2 q2) =
    Action (fwd1 `MS.union` fwd2) (q1 `MS.union` q2)
  mempty = dropPkt

-- |Shorthand for combining policies and actions.
(<+>) :: Monoid a => a -> a -> a
(<+>) = mappend 

(<||>) = PrUnion

(<&&>) = PrIntersect

neg = PrNegate

(==>) = PoBasic

-- |Construct the policy restricted by the predicate
policy <%> pred = case policy of
  PoBottom -> PoBottom
  PoBasic predicate act -> PoBasic (PrIntersect predicate pred) act
  PoUnion p1 p2 -> PoUnion (p1 <%> pred) (p2 <%> pred)

instance Monoid Policy where
  mappend = PoUnion
  mempty = PoBottom

-- |Match ethernet source address.
dlSrc     :: Word48     -> Predicate
dlSrc     value = PrPattern (top {ptrnDlSrc = exact value})

-- |Match ethernet destination address.
dlDst     :: Word48     -> Predicate
dlDst     value = PrPattern (top {ptrnDlDst = exact value})

-- |Match ethernet type code (e.g., 0x0800 for IP packets).
dlTyp     :: Word16     -> Predicate
dlTyp     value = PrPattern (top {ptrnDlTyp = exact value})

-- |Match VLAN tag.
dlVlan    :: Word16     -> Predicate
dlVlan    value = PrPattern (top {ptrnDlVlan = exact value})

-- |Match VLAN priority
dlVlanPcp :: Word8      -> Predicate
dlVlanPcp value = PrPattern (top {ptrnDlVlanPcp = exact value})

-- |Match source IP address.
--
-- This is only meaningful in combination with 'dlTyp 0x0800'.
nwSrc     :: Word32     -> Predicate
nwSrc     value = PrPattern (top {ptrnNwSrc = Prefix value 32})

-- |Match destination IP address.
nwDst     :: Word32     -> Predicate
nwDst     value = PrPattern (top {ptrnNwDst = Prefix value 32})

-- |Match a prefix of the source IP address.
nwSrcPrefix :: Word32 -> Int -> Predicate
nwSrcPrefix value prefix = PrPattern (top {ptrnNwSrc = Prefix value prefix})

-- |Match a prefix of the destination IP address.
nwDstPrefix :: Word32 -> Int -> Predicate
nwDstPrefix value prefix = PrPattern (top {ptrnNwDst = Prefix value prefix})

-- |Match IP protocol code (e.g., 0x6 indicates TCP segments).
nwProto   :: Word8      -> Predicate
nwProto   value = PrPattern (top {ptrnNwProto = exact value})

-- |Match IP TOS field.
nwTos     :: Word8      -> Predicate
nwTos     value = PrPattern (top {ptrnNwTos = exact value})

-- |Match IP source port.
tpSrc     :: Word16     -> Predicate
tpSrc     value = PrPattern (top {ptrnTpSrc = exact value})

-- |Match IP destination port.
tpDst     :: Word16     -> Predicate
tpDst     value = PrPattern (top {ptrnTpDst = exact value})

-- |Match the ingress port on which packets arrive.
inPort    :: Port       -> Predicate
inPort    value = PrPattern (top {ptrnInPort = exact value})

modDlSrc     value = unmodified {modifyDlSrc = Just value}
modDlDst     value = unmodified {modifyDlDst = Just value}
modDlVlan    value = unmodified {modifyDlVlan = Just value}
modDlVlanPcp value = unmodified {modifyDlVlanPcp = Just value}
modNwSrc     value = unmodified {modifyNwSrc = Just value}
modNwDst     value = unmodified {modifyNwDst = Just value}
modNwTos     value = unmodified {modifyNwTos = Just value}
modTpSrc     value = unmodified {modifyTpSrc = Just value}
modTpDst     value = unmodified {modifyTpDst = Just value}
