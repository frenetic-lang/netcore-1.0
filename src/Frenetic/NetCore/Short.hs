module Frenetic.NetCore.Short
  ( -- * Shorthand constructors
  -- ** Predicates
    inport
  , (<||>)
  , (<&&>)
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
import Frenetic.Common

-- |Construct the predicate matching packets on this switch and port
inport :: Switch -> Port -> Predicate
inport switch port = And (Switch switch) (IngressPort port)

-- |Construct the set difference between p1 and p2
prSubtract :: Predicate -> Predicate -> Predicate
prSubtract p1 p2 = And p1 (Not p2)

-- |Construct nary union of a list of predicates
prOr :: [Predicate] -> Predicate
prOr [] = None
prOr ps = List.foldr1 (\ p1 p2 -> Or p1 p2) ps

-- |Construct nary intersection of a list of predicates
prAnd :: [Predicate] -> Predicate
prAnd [] = Any
prAnd ps = List.foldr1 (\ p1 p2 -> And p1 p2) ps

dropPkt :: MultiSet Action
dropPkt = MS.empty

-- |Forward the packet out of all physical ports, except the packet's
-- ingress port.
allPorts :: Modification -- ^modifications to apply to the packet. Use
                         -- 'allPorts unmodified' to make no modifications.
         -> MultiSet Action
allPorts mod = MS.singleton (Forward AllPorts mod)

-- |Forward the packet out of the specified physical ports.
forward :: [Port] -> MultiSet Action
forward ports = MS.fromList lst
  where lst = [ Forward (Physical p) unmodified | p <- ports ]

-- |Forward the packet out of the specified physical ports with modifications.
--
-- Each port has its own record of modifications, so modifications at one port
-- do not interfere with modifications at another port.
modify :: [(Port, Modification)] -> MultiSet Action
modify mods = MS.fromList lst
  where lst = [ Forward (Physical p) mod | (p, mod) <- mods ]

-- |Join: overloaded to find the union of policies and the join of actions.
(<+>) :: Monoid a => a -> a -> a
(<+>) = mappend

-- |Abbreviation for predicate union.
(<||>) = Or

-- |Abbreviation for predicate intersection.
(<&&>) = And

-- |Abbreviation for constructing a basic policy from a predicate and an action.
(==>) = PoBasic

-- |Restrict a policy to act over packets matching the predicate.
policy <%> pred = case policy of
  PoBottom -> PoBottom
  PoBasic predicate act -> PoBasic (And predicate pred) act
  PoUnion p1 p2 -> PoUnion (p1 <%> pred) (p2 <%> pred)

instance Monoid Policy where
  mappend = PoUnion
  mempty = PoBottom





modDlSrc     value = unmodified {modifyDlSrc = Just value}
modDlDst     value = unmodified {modifyDlDst = Just value}
modDlVlan    value = unmodified {modifyDlVlan = Just value}
modDlVlanPcp value = unmodified {modifyDlVlanPcp = Just value}
modNwSrc     value = unmodified {modifyNwSrc = Just value}
modNwDst     value = unmodified {modifyNwDst = Just value}
modNwTos     value = unmodified {modifyNwTos = Just value}
modTpSrc     value = unmodified {modifyTpSrc = Just value}
modTpDst     value = unmodified {modifyTpDst = Just value}
