module Frenetic.NetCore.Short
  ( -- * Shorthand constructors
  -- ** Predicates
    inport
  , (<|>)
  , (<&>)
  , neg
  , prDifference
  , prNaryUnion
  -- ** Actions
  , dropPkt
  , flood
  , forward
  , forwardMods
  -- ** Policies
  , (==>)
  , (%)
  , (<+>)
  , poRestrict
  , poNaryUnion
  -- * Exact match predicate constructors
  , dlSrc
  , dlDst
  , dlTyp
  , dlVlan
  , dlVlanPcp
  , nwSrc
  , nwDst
  , nwProto
  , nwTos
  , tpSrc
  , tpDst
  , inPort
  -- * Exact match pattern constructors
  , patDlSrc
  , patDlDst
  , patDlTyp
  , patDlVlan
  , patDlVlanPcp
  , patNwSrc
  , patNwDst
  , patNwProto
  , patNwTos
  , patTpSrc
  , patTpDst
  , patInPort
  ) where

import Data.Word
import qualified Data.List as List
import qualified Data.MultiSet as MS
import Frenetic.Pattern
import Frenetic.NetCore.API

-- |Construct the predicate matching packets on this switch and port
inport :: Switch -> Port -> Predicate
inport switch port = PrIntersect (PrTo switch)
                                 (PrPattern (top {ptrnInPort = Exact port}))

-- |Construct the set difference between p1 and p2
prDifference :: Predicate -> Predicate -> Predicate
prDifference p1 p2 = PrIntersect p1 (PrNegate p2)

-- |Construct nary union of a list of predicates
prNaryUnion :: [Predicate] -> Predicate
prNaryUnion [] = PrNegate top
prNaryUnion ps = List.foldr1 (\ p1 p2 -> PrUnion p1 p2) ps

dropPkt :: Action
dropPkt = Action MS.empty []

flood :: Action
flood = Action (MS.singleton (PhysicalFlood, top)) []

forward :: Port -> Action
forward port = forwardMods [(port, top)]

forwardMods :: [(Port, Rewrite)] -> Action
forwardMods mods = Action (MS.fromList mods') [] where
  mods' = map (\(p, m) -> (Physical p, m)) mods

(<|>) = PrUnion
(<&>) = PrIntersect
neg = PrNegate

(==>) = PoBasic
(%) = poRestrict
(<+>) = PoUnion

-- |Construct the policy restricted by the predicate
poRestrict :: Policy -> Predicate -> Policy
poRestrict policy pred=
  case policy of
    PoBottom -> PoBottom
    PoBasic predicate act -> PoBasic (PrIntersect predicate pred) act
    PoUnion p1 p2 -> PoUnion (poRestrict p1 pred) (poRestrict p2 pred)

-- |Construct the union of a list of policies
poNaryUnion :: [Policy] -> Policy
poNaryUnion [] = PoBottom
poNaryUnion ps = List.foldr1 (\ p1 p2 -> PoUnion p1 p2) ps

dlSrc     :: Word48     -> Predicate
dlDst     :: Word48     -> Predicate
dlTyp     :: Word16     -> Predicate
dlVlan    :: Word16     -> Predicate
dlVlanPcp :: Word8      -> Predicate
nwSrc     :: Word32     -> Predicate
nwDst     :: Word32     -> Predicate
nwProto   :: Word8      -> Predicate
nwTos     :: Word8      -> Predicate
tpSrc     :: Word16     -> Predicate
tpDst     :: Word16     -> Predicate
inPort    :: Port       -> Predicate

dlSrc     value = PrPattern (top {ptrnDlSrc = exact value})
dlDst     value = PrPattern (top {ptrnDlDst = exact value})
dlTyp     value = PrPattern (top {ptrnDlTyp = exact value})
dlVlan    value = PrPattern (top {ptrnDlVlan = exact value})
dlVlanPcp value = PrPattern (top {ptrnDlVlanPcp = exact value})
nwSrc     value = PrPattern (top {ptrnNwSrc = Prefix value 32})
nwDst     value = PrPattern (top {ptrnNwDst = Prefix value 32})
nwProto   value = PrPattern (top {ptrnNwProto = exact value})
nwTos     value = PrPattern (top {ptrnNwTos = exact value})
tpSrc     value = PrPattern (top {ptrnTpSrc = exact value})
tpDst     value = PrPattern (top {ptrnTpDst = exact value})
inPort    value = PrPattern (top {ptrnInPort = Exact value})

patDlSrc     :: Word48     -> Pattern
patDlDst     :: Word48     -> Pattern
patDlTyp     :: Word16     -> Pattern
patDlVlan    :: Word16     -> Pattern
patDlVlanPcp :: Word8      -> Pattern
patNwSrc     :: Word32     -> Pattern
patNwDst     :: Word32     -> Pattern
patNwProto   :: Word8      -> Pattern
patNwTos     :: Word8      -> Pattern
patTpSrc     :: Word16     -> Pattern
patTpDst     :: Word16     -> Pattern
patInPort    :: Port       -> Pattern

patDlSrc     value = top {ptrnDlSrc = exact value}
patDlDst     value = top {ptrnDlDst = exact value}
patDlTyp     value = top {ptrnDlTyp = exact value}
patDlVlan    value = top {ptrnDlVlan = exact value}
patDlVlanPcp value = top {ptrnDlVlanPcp = exact value}
patNwSrc     value = top {ptrnNwSrc = Prefix value 32}
patNwDst     value = top {ptrnNwDst = Prefix value 32}
patNwProto   value = top {ptrnNwProto = exact value}
patNwTos     value = top {ptrnNwTos = exact value}
patTpSrc     value = top {ptrnTpSrc = exact value}
patTpDst     value = top {ptrnTpDst = exact value}
patInPort    value = top {ptrnInPort = Exact value}
