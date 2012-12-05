module Frenetic.NetCore.Compiler
  ( compile
  , compilePolicy
  , toFlowTable
  , compilePredicate
  , Bone (..) -- TODO(arjun): do not export
  , Classifier
  , classify
  , minimizeClassifier
  , classifierActions
  ) where

import Nettle.OpenFlow.Packet
import Nettle.OpenFlow.Match
import qualified Nettle.OpenFlow as OF
import Frenetic.Switches.OpenFlow
import Prelude hiding (pred)
import Frenetic.Pattern
import Frenetic.Common
import Frenetic.Topo (Switch,Port,Loc)
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
         -> Port
         -> Packet
         -> Classifier actn
         -> Maybe actn
classify switch pt pkt rules = foldl f Nothing rules where
    f (Just a) (ptrn, actn) = Just a
    f Nothing (ptrn, actn) = if matchPkt ptrn pt pkt
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
          in p1 == p2

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

inter :: (a -> a -> a) -> Skeleton a -> Skeleton a -> Skeleton a
inter f cf1 cf2 = cf3
  where (cf3, _, _) = skelCart f cf1 cf2

union :: (a -> a -> a) -> Skeleton a -> Skeleton a -> Skeleton a
union f cf1 cf2 = skelMinimize $ cf12 ++ cf1' ++ cf2'
  where (cf12, cf1', cf2') = skelCart f cf1 cf2

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
  Not pr -> skelMinimize $ skelMap not (pred sw pr) ++ [Bone matchAny True]

classifierActions :: Skeleton [Act] -> [Act]
classifierActions cl = concatMap f cl
  where f (Bone _ acts) = acts

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
-- compilePolicy sw (PolSeq pol1 pol2) = skelMinimize cf3
--   where cf1 = compilePolicy sw pol1
--         cf2 = compilePolicy sw pol2
--         acts1 = classifierActions cf1
--         cf2Preimages = map (\act1 -> mapMaybe (mkPreimg act1) cf2) acts1
--         mkPreimg act1 (Bone pat2 acts2) = case preimgOfAct act1 pat2 of
--           Just pat2' -> Just (Bone pat2' (catMaybes (map (seqAct act1) acts2)))
--           Nothing -> Nothing
--         cf1cf2 = map (inter (\_ rhs -> rhs) cf1) cf2Preimages
--         cf3 = case cf1cf2 of
--           [] -> []
--           (x:xs) -> foldr (union (<+>)) x xs
compilePolicy _ (PolRestrict (PolGenPacket _) _) = []
compilePolicy _ (PolGenPacket _) = []
compilePolicy s (PolRestrict pol pred) =
  compilePolicy s (synthRestrictPol pol pred)


{-| Compile a policy to a classifier. -}
compile :: Switch
        -> Pol
        -> Classifier [Act]
compile s po = map f skel ++ [(matchAny, [])]
  where
    f (Bone sptrn actn) = (sptrn, actn)
    skel = compilePolicy s po

useInPort (Just pt) (OF.SendOutPort (OF.PhysicalPort pt'))
  | pt == pt' = OF.SendOutPort OF.InPort
  | otherwise = OF.SendOutPort (OF.PhysicalPort pt')
useInPort _ act = act

toFlowTable :: Classifier [Act] -> IO [(OF.Match, OF.ActionSequence)]
toFlowTable classifier = mapM f classifier
  where
    f (m, a) = do
      newAct <- actnTranslate a
      return (m, map (useInPort (OF.inPort m)) $ newAct)
