module Frenetic.NetCore.Compiler
  ( compile
  , toFlowTable
  , compilePredicate
  , Bone (..) -- TODO(arjun): do not export
  , Classifier
  , classify
  , minimizeClassifier
  ) where

import Nettle.OpenFlow.Packet
import Nettle.OpenFlow.Match
import qualified Nettle.OpenFlow as OF
import Frenetic.Switches.OpenFlow
import Prelude hiding (pred)
import Frenetic.Pattern
import Frenetic.Common
import Frenetic.NetCore.Types
import Frenetic.NetCore.Semantics
import Frenetic.NetCore.Short
import Data.Dynamic
import qualified Data.List as List
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map

compilePredicate :: Switch
                 -> Predicate
                 -> Skeleton Bool
compilePredicate = pred

{-| Input: a function, a value, and two lists. Apply the function to each pair from the two lists and the current value. The function may modify the two lists and modify the current value. -}
cartMap :: (c -> a -> b -> (c, Maybe a, Maybe b)) -> c -> [a] -> [b] -> (c, [a], [b])
cartMap f c [] ys = (c, [], ys)
cartMap f c (x:xs) ys =
  let (c', xo, ys') = cartMap' c x ys in
  let (c'', xs', ys'') = cartMap f c' xs ys' in
  let xs'' = case xo of { Just x' -> x' : xs'; Nothing -> xs' } in
  (c'',xs'',ys'')
  where
    cartMap' c x [] = (c, Just x, [])
    cartMap' c x (y:ys) =
      case f c x y of
        (c', Just x', Just y') ->
           let (c'', xo', ys') = cartMap' c' x' ys in
           (c'', xo', y':ys')
        (c', Nothing, Just y') -> (c', Nothing, y':ys)
        (c', Just x', Nothing) -> cartMap' c' x' ys
        (c', Nothing, Nothing) -> (c', Nothing, ys)

{-| Classifiers are the target of compilation. -}
type Classifier a = [(Match, a)]

classify :: Switch
         -> PacketInfo
         -> Classifier actn
         -> Maybe actn
classify switch pkt rules = foldl f Nothing rules where
    f (Just a) (ptrn, actn) = Just a
    f Nothing (ptrn, actn) = if ptrnMatchPkt pkt ptrn
                             then Just actn
                             else Nothing

{-| Attempt to reduce the number of rules in the classifier.

      1. Remove a rule if it is a subset of a higher-priority rule: O(n^2).
      2. NYI

|-}
minimizeShadowing getPat rules = reverse $ f $ reverse rules
  where f []     = []
        f (x:xs) = if any (shadows x) xs
                   then f xs
                   else x:(f xs)
        shadows a1 a2 =
          let p1 = getPat a1
              p2 = getPat a2
          in case intersect p1 p2 of
            Nothing -> False
            Just p3 -> match p1 p3

minimizeClassifier :: Classifier a
                   -> Classifier a
minimizeClassifier rules = minimizeShadowing fst rules

{-| Each rule of the intermediate form is called a Bone. -}
data Bone actn = Bone Match actn
  deriving (Show, Eq)

{-| Skeletons are the intermediate form. -}
type Skeleton actn = [Bone actn]

{-| Map the actions. |-}
skelMap :: (a -> b) -> Skeleton a -> Skeleton b
skelMap f bones = map (\(Bone ptrn actns) -> Bone ptrn (f actns)) bones

{-| Cartesian combine two skeletons given a combination function for the actions. -}
skelCart :: (actn -> actn -> actn)
         -> Skeleton actn
         -> Skeleton actn
         -> (Skeleton actn,
             Skeleton actn,
             Skeleton actn)
skelCart f bs1 bs2 =
  let
    (bs1',bs2',bs3') = cartMap h [] bs1 bs2
  in
   (bs1', bs2', bs3')
  where
    h bs x@(Bone ptrn1 actns1) y@(Bone ptrn2 actns2) =
        case intersect ptrn1 ptrn2 of
          Just ptrn12 ->
            (bs ++ [Bone ptrn12 (f actns1 actns2)],
             if ptrn12 == ptrn1 then Nothing else Just x,
             if ptrn12 == ptrn2 then Nothing else Just y)
          Nothing ->
            (bs, Just x, Just y)

{-| Attempt to reduce the number of rules in a Skeleton. -}
skelMinimize :: Skeleton actn
             -> Skeleton actn
skelMinimize bones = minimizeShadowing getPat bones
  where getPat (Bone p1 as) = p1

{-| Compile a predicate to intermediate form. -}
pred :: Switch
     -> Predicate
     -> Skeleton Bool
pred sw pr = case pr of
  Any -> [Bone matchAny True]
  None -> [Bone matchAny False]
  DlSrc eth -> [Bone (matchAny { srcEthAddress = Just eth }) True]
  DlDst eth -> [Bone (matchAny { dstEthAddress = Just eth }) True]
  DlTyp typ -> [Bone (matchAny { ethFrameType = Just typ }) True]
  DlVlan Nothing -> [Bone (matchAny { vLANID = Just $ fromInteger ofpVlanNone }) True]
  DlVlan (Just vl) -> [Bone (matchAny { vLANID = Just vl }) True]
  DlVlanPcp pcp -> [Bone (matchAny { vLANPriority = Just pcp }) True]
  NwSrc ip -> [Bone (matchAny { srcIPAddress = ip }) True]
  NwDst ip -> [Bone (matchAny { dstIPAddress = ip }) True]
  NwProto proto -> [Bone (matchAny { matchIPProtocol = Just proto }) True]
  NwTos tos -> [Bone (matchAny { ipTypeOfService = Just tos }) True]
  TpSrcPort pt -> [Bone (matchAny { srcTransportPort = Just pt }) True]
  TpDstPort pt -> [Bone (matchAny { dstTransportPort = Just pt }) True]
  IngressPort pt -> [Bone (matchAny { inPort = Just pt }) True]
  Switch s' | sw == s' -> [Bone matchAny True]
          | otherwise -> []
  And pr1 pr2 -> skelMinimize skel12'
    where skel1 = pred sw pr1
          skel2 = pred sw pr2
          (skel12', skel1', skel2') = skelCart (&&) skel1 skel2
  Or pr1 pr2 -> skelMinimize $ skel12' ++ skel1' ++ skel2'
    where skel1 = pred sw pr1
          skel2 = pred sw pr2
          (skel12', skel1', skel2') = skelCart (||) skel1 skel2
  Not pr -> skelMap not (pred sw pr) ++ [Bone matchAny True]

{-| Compile a policy to intermediate form -}
compilePolicy :: Switch -> Pol -> Skeleton [Act]
compilePolicy _ PolEmpty = []
compilePolicy s (PolProcessIn po as) =
    skelMap f $ pred s po
      where f True = as
            f False = []
compilePolicy s (PolUnion po1 po2) =
   skelMinimize $ skel12' ++ skel1' ++ skel2'
      where skel1 = compilePolicy s po1
            skel2 = compilePolicy s po2
            (skel12', skel1', skel2') = skelCart (<+>) skel1 skel2
compilePolicy _ (PolRestrict (PolGenPacket _) _) = []
compilePolicy _ (PolGenPacket _) = []
compilePolicy s (PolRestrict pol pred) =
  compilePolicy s (synthRestrictPol pol pred)

{-| Compile a policy to a classifier. -}
compile :: Switch
        -> Pol
        -> Classifier [Act]
compile s po = map f skel
  where
    f (Bone sptrn actn) = (sptrn, actn)
    skel = compilePolicy s po

useInPort (Just pt) (OF.SendOutPort (OF.PhysicalPort pt'))
  | pt == pt' = OF.SendOutPort OF.InPort
  | otherwise = OF.SendOutPort (OF.PhysicalPort pt')
useInPort _ act = act

toFlowTable :: Classifier [Act] -> [(OF.Match, OF.ActionSequence)]
toFlowTable classifier =
  map (\(m, a) -> (m, map (useInPort (OF.inPort m)) $ actnTranslate a))
      classifier
