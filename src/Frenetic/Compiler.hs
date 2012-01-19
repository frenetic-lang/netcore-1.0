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

import Data.Map as Map
import Data.Set as Set
import Data.List as List
import Data.Typeable
import Data.Dynamic
  
import qualified Frenetic.Pattern as P
import Frenetic.Language
    
--
-- intermediate forms
--

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


data Bone pat a = Bone pat a
data Skeleton pat a = Skeleton [Bone pat a]

instance (Show pat, Show a) => Show (Bone pat a) where
  show (Bone m a) = "Bone(" ++ show m ++ "," ++ show a ++ ")"

instance (Show pat, Show a) => Show (Skeleton pat a) where
  show (Skeleton bs) = concat (intersperse "\n" $ Prelude.map show bs)

(+++) :: Skeleton pat a -> Skeleton pat a -> Skeleton pat a
(+++) (Skeleton bs1) (Skeleton bs2) = Skeleton (bs1 ++ bs2)

skelMap :: (a -> b) -> Skeleton pat a -> Skeleton pat b 
skelMap f (Skeleton bs) = Skeleton $ Prelude.map (\(Bone m a) -> Bone m (f a)) bs

skelCart :: (P.Pattern pat) =>
            (a -> a -> a)
         -> Skeleton pat a
         -> Skeleton pat a
         -> (Skeleton pat a, Skeleton pat a, Skeleton pat a)
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
compile s p = skelMap actTranslate (compilePolicy s p)

--
-- specialization
--
              
expandPredicate :: forall ptrn pkt. (Show ptrn, Typeable ptrn, Transmissionable ptrn pkt) =>
                   Transmission ptrn pkt
                -> Predicate
                -> Predicate
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
                Transmission ptrn pkt
             -> Policy
             -> Policy
expandPolicy tr (PoBasic pr as) = PoBasic (expandPredicate tr pr) as
expandPolicy tr (PoUnion po1 po2) = PoUnion (expandPolicy tr po1) (expandPolicy tr po2)
expandPolicy tr (PoIntersect po1 po2) = PoIntersect (expandPolicy tr po1) (expandPolicy tr po2)
expandPolicy tr (PoDifference po1 po2) = PoDifference (expandPolicy tr po1) (expandPolicy tr po2)

specialize :: (Show ptrn, Typeable ptrn, Transmissionable ptrn pkt, Actionable actn) =>
              Transmission ptrn pkt
           -> Policy
           -> Skeleton ptrn actn
specialize tr po = compile (trSwitch tr) (expandPolicy tr po)
