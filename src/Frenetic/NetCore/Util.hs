-- |Functions to transforma and inspect policies that are useful for building
-- controllers.
module Frenetic.NetCore.Util
  ( Field (..)
  , exactMatch
  , modifiedFields
  , modifyActions
  , policyImage
  , prUnIntersect
  , prUnUnion
  , poUnUnion
  , poDom
  , size
  , isForward
  , isGetPacket
  , isQuery
  ) where

import Frenetic.Common
import Frenetic.NetCore.Types
import qualified Data.List as List
import qualified Data.Set as Set
import Nettle.Ethernet.EthernetAddress
import Nettle.IPv4.IPAddress

-- |Names of common header fields.
data Field
  = FDlSrc | FDlDst | FDlVlan | FDlVlanPcp | FNwSrc | FNwDst | FNwTos 
  | FTpSrc | FTpDst | FNwProto | FInPort
  deriving (Eq, Ord, Show)

modifyActions :: ([Action] -> [Action]) 
              -> Policy 
              -> Policy
modifyActions f pol = case pol of
  PoBottom -> PoBottom
  PoBasic pred acts -> PoBasic pred (f acts)
  PoUnion pol1 pol2 -> PoUnion (modifyActions f pol1) (modifyActions f pol2)
  Restrict pol pred -> Restrict (modifyActions f pol) pred
  SendPackets _ -> pol

policyImage :: Policy
            -> [Action]
policyImage pol = case pol of
  PoBottom -> []
  PoBasic _ acts -> acts
  PoUnion pol1 pol2 -> policyImage pol1 ++ policyImage pol2
  Restrict pol pred -> policyImage pol
  SendPackets _ -> []

-- |Get back all predicates in the intersection.  Does not return any naked
-- intersections.
prUnIntersect :: Predicate -> [Predicate]
prUnIntersect po = List.unfoldr f [po] where
  f predicates = case predicates of
    [] -> Nothing
    (And p1 p2) : rest -> f (p1 : (p2 : rest))
    p : rest -> Just (p, rest)

-- |Get back all predicates in the union.  Does not return any naked unions.
prUnUnion :: Predicate -> [Predicate]
prUnUnion po = List.unfoldr f [po] where
  f predicates = case predicates of
    [] -> Nothing
    (Or p1 p2) : rest -> f (p1 : (p2 : rest))
    p : rest -> Just (p, rest)


-- |Get back all basic policies in the union.  Does not return any unions.
poUnUnion :: Policy -> [Policy]
poUnUnion po = List.unfoldr f [po] where
  f policies = case policies of
    [] -> Nothing
    (PoUnion p1 p2) : rest -> f (p1 : (p2 : rest))
    p : rest -> Just (p, rest)

-- |Returns a predicate that matches the domain of the policy.
poDom :: Policy -> Predicate
poDom PoBottom = None
poDom (PoBasic pred _) = pred
poDom (PoUnion pol1 pol2) = Or (poDom pol1) (poDom pol2)
poDom (Restrict pol pred) = And (poDom pol) (Not pred)
poDom (SendPackets _) = None -- TODO(arjun): but this has a range, right?

-- |Returns the approximate size of the policy
size :: Policy -> Int
size PoBottom = 1
size (PoBasic p _) = prSize p + 1
size (PoUnion p1 p2) = size p1 + size p2 + 1
size (Restrict pol pred) = size pol + prSize pred + 1
size (SendPackets _) = 1

-- |Returns the approximate size of the predicate
prSize :: Predicate -> Int
prSize (Or p1 p2) = prSize p1 + prSize p2 + 1
prSize (And p1 p2) = prSize p1 + prSize p2 + 1
prSize (Not p) = prSize p + 1
prSize _ = 1

-- |A predicate that exactly matches a packet's headers.
exactMatch :: Packet -> Predicate
exactMatch (Packet{..}) = foldl f None lst
  where f pr Nothing = pr
        f pr (Just pr') = And pr pr'
        lst = [ Just (DlSrc pktDlSrc), 
                Just (DlDst pktDlDst),
                Just (DlTyp pktDlTyp),
                Just (DlVlan pktDlVlan),
                Just (DlVlanPcp pktDlVlanPcp),
                case pktNwSrc of
                  Nothing -> Nothing
                  Just v -> Just (NwSrc (IPAddressPrefix v 32)),
                case pktNwDst of
                  Nothing -> Nothing
                  Just v -> Just (NwDst (IPAddressPrefix v 32)),
                Just (NwProto pktNwProto), 
                Just (NwTos pktNwTos), 
                fmap TpSrcPort pktTpSrc,
                fmap TpDstPort pktTpDst, 
                Just (IngressPort pktInPort) ]

modifiedFields :: Modification -> Set Field
modifiedFields (Modification{..}) = Set.fromList (catMaybes fields) where
  fields = [ case modifyDlSrc of { Just _ -> Just FDlSrc; Nothing -> Nothing }
           , case modifyDlDst of { Just _ -> Just FDlDst; Nothing -> Nothing }
           , case modifyDlVlan of { Just _ -> Just FDlVlan; Nothing -> Nothing }
           , case modifyDlVlanPcp of { Just _ -> Just FDlVlanPcp;
                                       Nothing -> Nothing }
           , case modifyNwSrc of { Just _ -> Just FNwSrc; Nothing -> Nothing }
           , case modifyNwDst of { Just _ -> Just FNwDst; Nothing -> Nothing }
           , case modifyNwTos of { Just _ -> Just FNwTos; Nothing -> Nothing }
           , case modifyTpSrc of { Just _ -> Just FTpSrc; Nothing -> Nothing }
           , case modifyTpDst of { Just _ -> Just FTpDst; Nothing -> Nothing }
           ]

isGetPacket (GetPacket{}) = True
isGetPacket _ = False

isForward :: Action -> Bool
isForward (Forward _ _) = True
isForward _ = False

isQuery :: Action -> Bool
isQuery act = case act of
  CountPackets {} -> True
  CountBytes {} -> True
  GetPacket {} -> True
  Forward {} -> False