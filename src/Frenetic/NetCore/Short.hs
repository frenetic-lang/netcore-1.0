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
  , crunch
  , size
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

import Debug.Trace

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


-- |Factor a policy by switch.  Returns an equivalent policy.
crunch :: Policy -> [Switch] -> Policy
-- crunch p ss = mconcat $ filter stripBots $ map (doCrunch p) ss
crunch p ss = trace ("Size before crunch: " ++ (show $ size p)) $ trace ("Size after crunch: " ++ (show $ size policy)) policy
  where
    policy = mconcat $ map (doCrunch p) ss
    doCrunch :: Policy -> Switch -> Policy
    doCrunch p s = case crunchP p s of
      PoBottom -> PoBottom
      p'       -> Restrict p' (Switch s)
    stripBots PoBottom = True
    stripBots _ = False

-- |Returns the policy fragment applicable to Switch.
crunchP :: Policy -> Switch -> Policy
crunchP PoBottom _ = PoBottom
crunchP (PoBasic pr as) s = 
  case crunchPr pr s of
    None -> PoBottom
    pr'  -> PoBasic pr' as
crunchP (PoUnion p1 p2) s =
  case (crunchP p1 s, crunchP p2 s) of
    (PoBottom, PoBottom) -> PoBottom
    (PoBottom, p2')      -> p2'
    (p1', PoBottom)      -> p1'
    (p1', p2')           -> PoUnion p1' p2'
crunchP (Restrict p pr) s =
  case (crunchP p s, crunchPr pr s) of
    (PoBottom, _)  -> PoBottom
    (_, None)      -> PoBottom
    (p', Any)      -> p'
    (p', pr')      -> Restrict p' pr'
crunchP (SendPackets c) s = SendPackets c
crunchP (Sequence p1 p2) s = 
  case (crunchP p1 s, crunchP p2 s) of
    (PoBottom, _)  -> PoBottom
    (_, PoBottom)  -> PoBottom
    (p1', p2')     -> Sequence p1' p2'
  
  -- |Specializes a predicate to a switch.
crunchPr :: Predicate -> Switch -> Predicate
crunchPr (Or p1 p2) s = 
  case (crunchPr p1 s, crunchPr p2 s) of
    (Any, p)     -> Any
    (p, Any)     -> Any
    (None, None) -> None
    (None, p)    -> p
    (p, None)    -> p
    (p1', p2')   -> Or p1' p2'
crunchPr (And p1 p2) s = 
  case (crunchPr p1 s, crunchPr p2 s) of
    (Any, p)     -> p
    (p, Any)     -> p
    (None, None) -> None
    (None, p)    -> None
    (p, None)    -> None
    (p1', p2')   -> And p1' p2'
crunchPr (Not p) s = 
  case crunchPr p s of
    None -> Any
    Any  -> None
    p'   -> Not p'
crunchPr (Switch s) s' | s == s' = Any
                       | s /= s' = None
crunchPr p s = p

size :: Policy -> Int
size PoBottom = 1
size (PoBasic p _) = prSize p + 1
size (PoUnion p1 p2) = size p1 + size p2 + 1
size (Restrict p pr) = size p + prSize pr + 1
size (SendPackets _) = 1
size (Sequence p1 p2) = size p1 + size p2 + 1

prSize :: Predicate -> Int
prSize (Or p1 p2) = prSize p1 + prSize p2 + 1
prSize (And p1 p2) = prSize p1 + prSize p2 + 1
prSize (Not p) = prSize p + 1
prSize _ = 1


