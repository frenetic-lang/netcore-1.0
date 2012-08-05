module Frenetic.NetCore.Compiler
  ( compile
  , compilePredicate
  , Bone (..) -- TODO(arjun): do not export
  , Classifier
  , classify
  , minimizeClassifier
  ) where

import           Frenetic.Compat
import           Frenetic.Pattern
import           Frenetic.Common
import           Frenetic.NetCore.Types
import           Frenetic.NetCore.Semantics
import           Frenetic.NetCore.Short
import           Data.Dynamic
import qualified Data.List            as List
import           Data.Maybe
import qualified Data.Set             as Set
import qualified Data.Map             as Map

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
type Classifier ptrn a = [(ptrn, a)]

classify :: FreneticImpl a
         => Switch
         -> PacketImpl a
         -> Classifier (PatternImpl a) actn
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
minimizeShadowing :: FreneticImpl a
                  => (b -> PatternImpl a)
                  -> [b]
                  -> [b]
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

minimizeClassifier :: FreneticImpl a
                   => Classifier (PatternImpl a) (ActionImpl a)
                   -> Classifier (PatternImpl a) (ActionImpl a)
minimizeClassifier rules = minimizeShadowing fst rules

{-| Each rule of the intermediate form is called a Bone. -}
data Bone ptrn actn = Bone ptrn Pattern actn
  deriving (Show, Eq)

{-| Skeletons are the intermediate form. -}
type Skeleton ptrn actn = [Bone ptrn actn]

{-| Map the actions. |-}
skelMap :: (a -> b) -> Skeleton ptrn a -> Skeleton ptrn b
skelMap f bones = map (\(Bone ptrn pr actns) -> Bone ptrn pr (f actns)) bones

{-| Cartesian combine two skeletons given a combination function for the actions. -}
skelCart :: FreneticImpl a
         => (actn -> actn -> actn)
         -> Skeleton (PatternImpl a) actn
         -> Skeleton (PatternImpl a) actn
         -> (Skeleton (PatternImpl a) actn,
             Skeleton (PatternImpl a) actn,
             Skeleton (PatternImpl a) actn)
skelCart f bs1 bs2 =
  let
    (bs1',bs2',bs3') = cartMap h [] bs1 bs2
  in
   (bs1', bs2', bs3')
  where
    h bs x@(Bone ptrn1 iptrn1 actns1) y@(Bone ptrn2 iptrn2 actns2) =
        case intersect ptrn1 ptrn2 of
          Just ptrn12 ->
            case intersect iptrn1 iptrn2 of
              Just iptrn12 ->
                (bs ++ [Bone ptrn12 iptrn12 (f actns1 actns2)],
                 if ptrn12 == ptrn1 then Nothing else Just x,
                 if ptrn12 == ptrn2 then Nothing else Just y)
              Nothing -> error "skelCart: i-pattern intersection failed."
          Nothing ->
            (bs, Just x, Just y)

{-| Attempt to reduce the number of rules in a Skeleton. -}
skelMinimize :: FreneticImpl a
             => Skeleton (PatternImpl a) actn
             -> Skeleton (PatternImpl a) actn
skelMinimize bones = minimizeShadowing getPat bones
  where getPat (Bone p1 p2 as) = FreneticPat p2

{-| Compile a predicate to intermediate form. -}
compilePredicate :: FreneticImpl a
                 => Switch
                 -> Predicate
                 -> Skeleton (PatternImpl a) Bool
compilePredicate s (PrPattern pat) = [Bone (fromPattern pat) pat True]
compilePredicate s (PrTo s') | s == s' = [Bone top top True]
                             | otherwise = []
compilePredicate s (PrIntersect pr1 pr2) = skelMinimize skel12'
    where
      skel1 = compilePredicate s pr1
      skel2 = compilePredicate s pr2
      (skel12', skel1', skel2') = skelCart (&&) skel1 skel2
compilePredicate s (PrUnion pr1 pr2) = skelMinimize $ skel12' ++ skel1' ++ skel2'
    where
      skel1 = compilePredicate s pr1
      skel2 = compilePredicate s pr2
      (skel12', skel1', skel2') = skelCart (||) skel1 skel2
compilePredicate s (PrNegate pr) =
  skelMap not (compilePredicate s pr) ++ [Bone top top True]

{-| Compile a policy to intermediate form -}
compilePolicy :: FreneticImpl a
              => Switch -> Policy -> Skeleton (PatternImpl a) Action
compilePolicy _ PoBottom = []
compilePolicy s (PoBasic po as) =
    skelMap f $ compilePredicate s po
      where f True = as
            f False = dropPkt
compilePolicy s (PoUnion po1 po2) =
   skelMinimize $ skel12' ++ skel1' ++ skel2'
      where skel1 = compilePolicy s po1
            skel2 = compilePolicy s po2
            (skel12', skel1', skel2') =
              skelCart (<+>) skel1 skel2

{-| Compile a policy to a classifier. -}
compile :: FreneticImpl a
        => Switch
        -> Policy
        -> Classifier (PatternImpl a) (ActionImpl a)
compile s po = map f skel
  where
    f (Bone sptrn iptrn actn)
      | toPattern sptrn `match` iptrn = (sptrn, actnTranslate actn)
      | otherwise = (sptrn, actnController)
    skel = compilePolicy s po

