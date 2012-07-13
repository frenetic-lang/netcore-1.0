--------------------------------------------------------------------------------
-- The Frenetic Project                                                       --
-- frenetic@frenetic-lang.org                                                 --
--------------------------------------------------------------------------------
-- Licensed to the Frenetic Project by one or more contributors. See the      --
-- NOTICE file distributed with this work for additional information          --
-- regarding copyright and ownership. The Frenetic Project licenses this      --
-- file to you under the following license.                                   --
--                                                                            --
-- Redistribution and use in source and binary forms, with or without         --
-- modification, are permitted provided the following conditions are met:     --
-- - Redistributions of source code must retain the above copyright           --
--   notice, this list of conditions and the following disclaimer.            --
-- - Redistributions of binaries must reproduce the above copyright           --
--   notice, this list of conditions and the following disclaimer in          --
--   the documentation or other materials provided with the distribution.     --
-- - The names of the copyright holds and contributors may not be used to     --
--   endorse or promote products derived from this work without specific      --
--   prior written permission.                                                --
--                                                                            --
-- Unless required by applicable law or agreed to in writing, software        --
-- distributed under the License is distributed on an "AS IS" BASIS, WITHOUT  --
-- WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the   --
-- LICENSE file distributed with this work for specific language governing    --
-- permissions and limitations under the License.                             --
--------------------------------------------------------------------------------
-- /src/Compiler.hs                                                           --
-- NetCore compiler                                                           --
--------------------------------------------------------------------------------

{-# LANGUAGE
    TemplateHaskell,
    MultiParamTypeClasses
 #-}
module Frenetic.NetCore.Compiler
  ( compile
  , compilePredicate
  , Bone (..) -- TODO(arjun): do not export
  , Skeleton (..) -- TODO(arjun): do not export
  , Classifier (..)
  , classify
  , minimizeClassifier
  ) where

import           Frenetic.Compat
import           Frenetic.Pattern
import           Frenetic.Util
import           Frenetic.NetCore.API
import Frenetic.NetCore.Semantics
import           Control.Newtype.TH
import           Control.Newtype
import           Data.Dynamic
import qualified Data.List            as List
import           Data.Maybe
import qualified Data.Set             as Set
import qualified Data.Map             as Map

{-| Input: a function, a context, a value, and a lists. Apply the function to each pair from the list and the context and current value; we may modify the list and current value. The context is the list of items we have already processed. -}
selfMap :: ([a] -> c -> a -> a -> (c, Maybe a, Maybe a)) -> [a] -> c -> [a] -> [a]
selfMap f ctxt c [] = []
selfMap f ctxt c (x : xs) =
    case selfMap' c x xs of
      (Just x', xs') -> x' : selfMap f (x' : ctxt)  c xs'
      (Nothing, xs') -> selfMap f ctxt c xs'
    where
      selfMap' c x [] = (Just x, [])
      selfMap' c x (y : ys) =
          case f ctxt c x y of
          (c', Just x', Just y') ->
              let
                  (jx, ys') = selfMap' c' x' ys
              in
                (jx, y' : ys')
          (c', Nothing, Just y') -> (Nothing, y' : ys)
          (c', Just x', Nothing) -> selfMap' c' x' ys
          (c', Nothing, Nothing) -> (Nothing, ys)

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
newtype Classifier ptrn a = Classifier [(ptrn, a)]
$(mkNewTypes [''Classifier])

-- We can show classifiers. TODO: Make the display nicer.
instance (Show ptrn, Show actn) => Show (Classifier ptrn actn) where
  show = List.intercalate "\n" . List.map show . unpack

classify :: FreneticImpl a
         => Switch
         -> PacketImpl a
         -> Classifier (PatternImpl a) actn
         -> Maybe actn
classify switch pkt (Classifier rules) = foldl f Nothing rules where
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
minimizeClassifier (Classifier rules) = Classifier $ minimizeShadowing fst rules

{-| Each rule of the intermediate form is called a Bone. -}
data Bone ptrn actn = Bone ptrn Pattern actn
  deriving (Show, Eq)

{-| Skeletons are the intermediate form. -}
newtype Skeleton ptrn actn = Skeleton [Bone ptrn actn]
                           deriving (Eq)
$(mkNewTypes [''Skeleton])

-- TODO: make this nicer.
instance (Show ptrn, Show actn) => Show (Skeleton ptrn actn) where
  show = List.intercalate "\n" . List.map show . unpack

{-| Concatenate two intermediate forms. -}
skelAppend :: Skeleton ptrn actn -> Skeleton ptrn actn -> Skeleton ptrn actn
skelAppend = newLift2 (++)

{-| Map the actions. |-}
skelMap :: (a -> b) -> Skeleton ptrn a -> Skeleton ptrn b
skelMap f (Skeleton bones) =
  Skeleton $ map (\(Bone ptrn pr actns) -> Bone ptrn pr (f actns)) bones

{-| Cartesian combine two skeletons given a combination function for the actions. -}
skelCart :: FreneticImpl a
         => (actn -> actn -> actn)
         -> Skeleton (PatternImpl a) actn
         -> Skeleton (PatternImpl a) actn
         -> (Skeleton (PatternImpl a) actn,
             Skeleton (PatternImpl a) actn,
             Skeleton (PatternImpl a) actn)
skelCart f (Skeleton bs1) (Skeleton bs2) =
  let
    (bs1',bs2',bs3') = cartMap h [] bs1 bs2
  in
   (Skeleton bs1', Skeleton bs2', Skeleton bs3')
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
skelMinimize (Skeleton bones) = Skeleton $ minimizeShadowing getPat bones
  where getPat (Bone p1 p2 as) = FreneticPat p2

{-| Compile a predicate to intermediate form. -}
compilePredicate :: FreneticImpl a
                 => Switch
                 -> Predicate
                 -> Skeleton (PatternImpl a) Bool
compilePredicate s (PrPattern pat) =
  Skeleton [Bone (fromPattern pat) pat True]
compilePredicate s (PrTo s') | s == s' = Skeleton [Bone top top True]
                             | otherwise = Skeleton []
compilePredicate s (PrIntersect pr1 pr2) = skelMinimize skel12'
    where
      skel1 = compilePredicate s pr1
      skel2 = compilePredicate s pr2
      (skel12', skel1', skel2') = skelCart (&&) skel1 skel2
compilePredicate s (PrUnion pr1 pr2) = skelMinimize $ skel12' `skelAppend` skel1' `skelAppend` skel2'
    where
      skel1 = compilePredicate s pr1
      skel2 = compilePredicate s pr2
      (skel12', skel1', skel2') = skelCart (||) skel1 skel2
compilePredicate s (PrNegate pr) =
  skelMap not (compilePredicate s pr) `skelAppend` Skeleton [Bone top top True]

{-| Compile a policy to intermediate form -}
compilePolicy :: FreneticImpl a
              => Switch -> Policy -> Skeleton (PatternImpl a) Action
compilePolicy _ PoBottom = Skeleton []
compilePolicy s (PoBasic po as) =
    skelMap f $ compilePredicate s po
      where f True = as
            f False = dropPkt
compilePolicy s (PoUnion po1 po2) =
   skelMinimize $ skel12' `skelAppend` skel1' `skelAppend` skel2'
      where skel1 = compilePolicy s po1
            skel2 = compilePolicy s po2
            (skel12', skel1', skel2') =
              skelCart unionAction skel1 skel2

{-| Compile a policy to a classifier. -}
compile :: FreneticImpl a
        => Switch
        -> Policy
        -> Classifier (PatternImpl a) (ActionImpl a)
compile s po = Classifier $ map f $ unpack skel
  where
    f (Bone sptrn iptrn actn)
      | toPattern sptrn `match` iptrn = (sptrn, actnTranslate actn)
      | otherwise = (sptrn, actnController)
    skel = compilePolicy s po

