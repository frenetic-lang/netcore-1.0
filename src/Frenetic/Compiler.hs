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
-- * Redistributions of source code must retain the above copyright           --
--   notice, this list of conditions and the following disclaimer.            --
-- * Redistributions of binaries must reproduce the above copyright           --
--   notice, this list of conditions and the following disclaimer in          --
--   the documentation or other materials provided with the distribution.     --
-- * The names of the copyright holds and contributors may not be used to     --
--   endorse or promote products derived from this work without specific      --
--   prior written permission.                                                --
--                                                                            --
-- Unless required by applicable law or agreed to in writing, software        --
-- distributed under the License is distributed on an "AS IS" BASIS, WITHOUT  --
-- WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the   --
-- LICENSE file distributed with this work for specific language governing    --
-- permissions and limitations under the License.                             --
--------------------------------------------------------------------------------
-- /src/Compiler.hs                                                            --
-- NetCore compiler                                                           --
-- $Id$ --
--------------------------------------------------------------------------------

{-# LANGUAGE 
    NoMonomorphismRestriction, 
    ExistentialQuantification,
    ImpredicativeTypes,
    ScopedTypeVariables,
    FlexibleInstances,
    TypeSynonymInstances, 
    GADTs
 #-}

module Frenetic.Compiler where

import Data.Set as Set
import Data.List as List
import Data.Maybe
import Data.Typeable
import Data.Dynamic
  
import qualified Frenetic.Pattern as P
import Frenetic.Language
    
--
-- strange n^2 list algorithms
--

-- f is your self map function
-- ctxt is everything we've seen before
-- c is a user value: takes the ctxt, user value, and the two list items we are considering. Then it returns the new user value, and whether we should keep any of the other values.
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

cartMap :: (c -> a -> b -> (c, Maybe a, Maybe b)) -> c -> [a] -> [b] -> (c, [a], [b])
cartMap f c [] ys = (c, [], ys)
cartMap f c (x:xs) ys = 
  let (c', xo, ys') = cartMap' c x ys in 
  let (c'', xs', ys'') = cartMap f c' xs ys' in 
  let xs'' = case xo of { Just x' -> (x' : xs'); Nothing -> xs' } in 
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

-- 
-- Skeletons
--

data Bone ptrn a = Bone ptrn a
data Skeleton ptrn a = Skeleton [Bone ptrn a]

instance (Show ptrn, Show a) => Show (Bone ptrn a) where
  show (Bone m a) = "Bone(" ++ show m ++ "," ++ show a ++ ")"

instance (Show ptrn, Show a) => Show (Skeleton ptrn a) where
  show (Skeleton bs) = concat (intersperse "\n" $ Prelude.map show bs)

(+++) :: Skeleton ptrn a -> Skeleton ptrn a -> Skeleton ptrn a
(+++) (Skeleton bs1) (Skeleton bs2) = Skeleton (bs1 ++ bs2)

skelMap :: (a -> b) -> Skeleton ptrn a -> Skeleton ptrn b 
skelMap f (Skeleton bs) = Skeleton $ Prelude.map (\(Bone m a) -> Bone m (f a)) bs

skelCart :: (P.Pattern ptrn) =>
            (a -> a -> a)
         -> Skeleton ptrn a
         -> Skeleton ptrn a
         -> (Skeleton ptrn a, Skeleton ptrn a, Skeleton ptrn a)
skelCart f (Skeleton bs1) (Skeleton bs2) = 
  let h bs x@(Bone m1 a1) y@(Bone m2 a2) = 
        case P.intersect m1 m2 of 
          Just m12 -> 
            (bs ++ [Bone m12 (f a1 a2)],
             if m12 == m1 then Nothing else Just x,
             if m12 == m2 then Nothing else Just y)
          Nothing -> 
            (bs, Just x, Just y) in 
  let (bs1',bs2',bs3') = cartMap h [] bs1 bs2 in 
  (Skeleton bs1', Skeleton bs2', Skeleton bs3')
  
{-| Minimize. |-}
skelMinimize :: (Patternable ptrn, Actionable actn) => Skeleton ptrn actn -> Skeleton ptrn actn
skelMinimize (Skeleton bones) = Skeleton bones''
    where
      bones' = selfMap f [] False bones
      f ctxt isDisjoint bone1@(Bone ptrn act) bone2@(Bone ptrn' act')
          -- Remove shadows
          | ptrn' `P.match` ptrn = (isDisjoint, Just bone1, Nothing)
          -- Remove fallthroughs
          | ptrn `P.match` ptrn' && act == act' && not isDisjoint =
              (False, Nothing, Just bone2) 
          | otherwise = (isDisjoint || (act /= act' && conflict),
                         Just bone1,
                         Just bone2)
          where
            ctxtmeets = mapMaybe (P.intersect ptrn) (Prelude.map (\(Bone ptrn actn) -> ptrn) ctxt)
            conflict = P.overlap ptrn ptrn' && P.unsafeIntersect ptrn ptrn' `List.notElem` ctxtmeets
                              
      -- remove bottom elements from end of classifier
      bones'' = reverse $ dropWhile (\(Bone ptrn actn) -> actn == actController) $ reverse bones'

{- Pruning -}
skelPrune :: (Transmissionable ptrn pkt, Actionable act) =>
             pkt -> Skeleton ptrn act -> Skeleton ptrn act
skelPrune pkt (Skeleton bones) = Skeleton $ removeControllers [] bones
    where
      removeControllers prefix [] = []
      removeControllers prefix (Bone ptrn actn : bones) 
          | not $ patMatch ptrn pkt = removeControllers prefix bones
          | actn == actController = removeControllers (ptrn : prefix) bones
          | any (P.overlap ptrn) prefix = removeControllers (ptrn : prefix) bones
          | otherwise = Bone ptrn actn : removeControllers prefix bones

--
-- compile
--
  
compilePredicate :: forall ptrn. (Typeable ptrn, Patternable ptrn) => Switch -> Predicate -> Skeleton ptrn Bool 
compilePredicate s (PrHeader h w) = Skeleton [Bone (patOverapprox h w) True]
compilePredicate s (PrPattern _ dyn) =
    case fromDynamic dyn :: Maybe ptrn of
      Just ptrn -> Skeleton [Bone ptrn True]
      Nothing -> Skeleton []
compilePredicate s (PrInport n) = Skeleton [Bone (patInport n) True] 
compilePredicate s (PrTo s') | s == s' = Skeleton [Bone P.top True]
                             | otherwise = Skeleton []
compilePredicate s (PrIntersect pr1 pr2) = skel12'
    where
      skel1 = compilePredicate s pr1
      skel2 = compilePredicate s pr2
      (skel12', skel1', skel2') = skelCart (&&) skel1 skel2
compilePredicate s (PrUnion pr1 pr2) = skel12' +++ skel1' +++ skel2'
    where
      skel1 = compilePredicate s pr1
      skel2 = compilePredicate s pr2
      (skel12', skel1', skel2') = skelCart (||) skel1 skel2
compilePredicate s (PrNegate pr) = skelMap not (compilePredicate s pr) +++
                                   Skeleton [Bone P.top True]

compilePolicy :: (Patternable ptrn, Typeable ptrn) => Switch -> Policy -> Skeleton ptrn Frenetic.Language.Actions
compilePolicy s (PoBasic po as) = 
    skelMap (\b -> if b then as else Set.empty) $ compilePredicate s po
compilePolicy s (PoUnion po1 po2) = skel12' +++ skel1' +++ skel2' 
    where
      skel1 = compilePolicy s po1
      skel2 = compilePolicy s po2
      (skel12', skel1', skel2') = skelCart Set.union skel1 skel2
compilePolicy s (PoIntersect po1 po2) = skel12'
    where
      skel1 = compilePolicy s po1
      skel2 = compilePolicy s po2
      (skel12', skel1', skel2') = skelCart Set.intersection skel1 skel2

compile :: (Typeable ptrn, Patternable ptrn, Actionable actn) => Switch -> Policy -> Skeleton ptrn actn 
compile s p = skelMinimize $ skelMap actTranslate (compilePolicy s p)

--
-- specialization
--
              
expandPredicate :: forall ptrn pkt. (Show ptrn, Typeable ptrn, Transmissionable ptrn pkt) =>
                   Transmission ptrn pkt -> Predicate -> Predicate
expandPredicate tr (PrHeader h w) =
    case (patUnderapprox h w (pktGetHeader (trPkt tr) h) :: Maybe ptrn) of
      Just pat -> PrUnion (PrHeader h w) (PrPattern (show pat) (toDyn pat))
      Nothing -> PrHeader h w
expandPredicate tr (PrPattern s dyn) = PrPattern s dyn
expandPredicate tr (PrTo s) = PrTo s
expandPredicate tr (PrInport p) = PrInport p
expandPredicate tr (PrUnion pr1 pr2) =
    PrUnion (expandPredicate tr pr1) (expandPredicate tr pr2)
expandPredicate tr (PrIntersect pr1 pr2) =
    PrIntersect (expandPredicate tr pr1) (expandPredicate tr pr2)
expandPredicate tr (PrDifference pr1 pr2) =
    PrDifference (expandPredicate tr pr1) (expandPredicate tr pr2)
expandPredicate tr (PrNegate pr) = PrNegate (expandPredicate tr pr)

expandPolicy :: (Show ptrn, Typeable ptrn, Transmissionable ptrn pkt) =>
                Transmission ptrn pkt -> Policy -> Policy
expandPolicy tr (PoBasic pr as) = PoBasic (expandPredicate tr pr) as
expandPolicy tr (PoUnion po1 po2) = PoUnion (expandPolicy tr po1) (expandPolicy tr po2)
expandPolicy tr (PoIntersect po1 po2) = PoIntersect (expandPolicy tr po1) (expandPolicy tr po2)
expandPolicy tr (PoDifference po1 po2) = PoDifference (expandPolicy tr po1) (expandPolicy tr po2)

specialize :: (Show ptrn, Typeable ptrn, Transmissionable ptrn pkt, Actionable actn) =>
              Transmission ptrn pkt -> Policy -> Skeleton ptrn actn
specialize tr po = skelPrune (trPkt tr) $ compile (trSwitch tr) (expandPolicy tr po)

