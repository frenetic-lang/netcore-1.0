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
  , synthRestrict
  -- * Packet modifications
  , Modification (..)
  , unmodified
  , modDlSrc
  , modDlDst
  , modDlVlan
  , stripDlVlan    
  , modDlVlanPcp
  , modNwSrc
  , modNwDst
  , modNwTos
  , modTpSrc
  , modTpDst
  ) where

import Frenetic.Topo (Switch,Port)
import Data.Word
import qualified Data.List as List
import Frenetic.Pattern
import Frenetic.NetCore.Types
import Frenetic.Common

infixr 5 <+>
infixl 6 ==>
infixr 7 <||>
infixr 8 <&&>

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

dropPkt :: [Action]
dropPkt = []

-- |Forward the packet out of all physical ports, except the packet's
-- ingress port.
allPorts :: Modification -- ^modifications to apply to the packet. Use
                         -- 'allPorts unmodified' to make no modifications.
         -> [Action]
allPorts mod = [Forward AllPorts mod]

-- |Forward the packet out of the specified physical ports.
forward :: [Port] -> [Action]
forward ports = [ Forward (Physical p) unmodified | p <- ports ]

-- |Forward the packet out of the specified physical ports with modifications.
--
-- Each port has its own record of modifications, so modifications at one port
-- do not interfere with modifications at another port.
modify :: [(Port, Modification)] -> [Action]
modify mods = [ Forward (Physical p) mod | (p, mod) <- mods ]

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
(<%>) = Restrict

-- |Restricts the policy's domain to 'pred'. Does not eliminate
-- 'Restrict' expressions, but does restrict their restrictions.
synthRestrict :: Policy -> Predicate -> Policy
synthRestrict pol pred = case pol of
  PoBottom -> PoBottom
  PoBasic pred' acts -> 
    PoBasic (And pred' pred) acts
  PoUnion pol1 pol2 ->
    PoUnion (synthRestrict pol1 pred) (synthRestrict pol2 pred)
  Sequence pol1 pol2 ->
    Sequence (synthRestrict pol1 pred) pol2
  Restrict (SendPackets chan) pred' -> 
    Restrict (SendPackets chan) (And pred pred')
  Restrict pol pred' -> 
    Restrict pol (And pred pred')
  SendPackets chan -> 
    Restrict (SendPackets chan) pred

instance Monoid Policy where
  mappend = PoUnion
  mempty = PoBottom

modDlSrc     value = unmodified {modifyDlSrc = Just value}
modDlDst     value = unmodified {modifyDlDst = Just value}
modDlVlan    value = unmodified {modifyDlVlan = Just value}
stripDlVlan    = unmodified {modifyDlVlan = Just Nothing}
modDlVlanPcp value = unmodified {modifyDlVlanPcp = Just value}
modNwSrc     value = unmodified {modifyNwSrc = Just value}
modNwDst     value = unmodified {modifyNwDst = Just value}
modNwTos     value = unmodified {modifyNwTos = Just value}
modTpSrc     value = unmodified {modifyTpSrc = Just value}
modTpDst     value = unmodified {modifyTpDst = Just value}
