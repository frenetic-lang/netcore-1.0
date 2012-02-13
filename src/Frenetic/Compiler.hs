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
    GADTs,
TemplateHaskell,
MultiParamTypeClasses
 #-}

module Frenetic.Compiler where

import           Frenetic.Util
import           Control.Newtype.TH
import           Control.Newtype

import           Data.Dynamic
import qualified Data.List          as List
import           Data.Maybe
import qualified Data.Set           as Set
import qualified Data.Map           as Map
import           Data.Typeable
  
import           Frenetic.Pattern
import           Frenetic.Language
    

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

{-
 Classifiers
-}

{-| Target of compilation -}
newtype Classifier ptrn a = Classifier [(ptrn, a)]
$(mkNewTypes [''Classifier])

instance (Show ptrn, Show a) => Show (Classifier ptrn a) where
  show = concat . List.intersperse "\n" . List.map show . unpack 

class (GPattern ptrn, GAction actn) => ValidClassifier ptrn actn
instance (GPattern ptrn, GAction actn) => ValidClassifier ptrn actn


class (ValidClassifier ptrn actn, ValidTransmission ptrn pkt) => ValidEnvironment ptrn actn pkt
instance (ValidClassifier ptrn actn, ValidTransmission ptrn pkt) => ValidEnvironment ptrn actn pkt


{-| Try to reduce the number of rules in the classifier. |-}
minimize :: (ValidClassifier ptrn actn) => Classifier ptrn actn -> Classifier ptrn actn
minimize = undefined

{-| Removes any rules that (1) have controller rules, (2) don't match the packet, (3) overlap with a rule that we previously removed.-}
prune :: (ValidEnvironment ptrn actn pkt) =>
         pkt -> Classifier ptrn actn -> Classifier ptrn actn
prune pkt = newLift $ removeControllers []
    where
      removeControllers prefix [] = []
      removeControllers prefix ((ptrn, actn) : rules) 
          | not $ ptrnMatchPkt pkt ptrn  = removeControllers prefix rules
          | actn == actnController = removeControllers (ptrn : prefix) rules
          | any (overlap ptrn) prefix = removeControllers (ptrn : prefix) rules
          | otherwise = (ptrn, actn) : removeControllers prefix rules


{-| The intermediate form -} 
data Bone ptrn actn = Bone ptrn Pattern (actn, actn)
                    deriving (Show, Eq)
newtype Skeleton ptrn actn = Skeleton [Bone ptrn actn]
                           deriving (Eq)
$(mkNewTypes [''Skeleton])

instance (Show ptrn, Show actn) => Show (Skeleton ptrn actn) where
  show = concat . List.intersperse "\n" . List.map show . unpack 

(+++) = newLift2 (++)

skelMap :: ((a, a) -> (b, b)) -> Skeleton ptrn a -> Skeleton ptrn b 
skelMap f (Skeleton bones) = Skeleton $ map (\(Bone ptrn pr actns) -> Bone ptrn pr (f actns)) bones 

skelCart :: (GPattern ptrn) =>
            ((a, a) -> (a, a) -> (a, a))
            -> Skeleton ptrn a
            -> Skeleton ptrn a
            -> (Skeleton ptrn a, Skeleton ptrn a, Skeleton ptrn a)
skelCart f (Skeleton bs1) (Skeleton bs2) = 
  let 
    (bs1',bs2',bs3') = cartMap h [] bs1 bs2 
  in 
   (Skeleton bs1', Skeleton bs2', Skeleton bs3')
  where
    h bs x@(Bone ptrn1 iptrn1 actns1) y@(Bone ptrn2 iptrn2 actns2) = 
        case intersect ptrn1 ptrn2 of 
          Just ptrn12 -> 
            -- FIX. No unsafe!
            (bs ++ [Bone ptrn12 (unsafeIntersect iptrn1 iptrn2) (f actns1 actns2)],
             if ptrn12 == ptrn1 then Nothing else Just x,
             if ptrn12 == ptrn2 then Nothing else Just y)
          Nothing -> 
            (bs, Just x, Just y) 
      
skelMinimize :: (ValidClassifier ptrn actn) => Skeleton ptrn actn -> Skeleton ptrn actn
skelMinimize = id

-- {-| Minimize. |-}
-- skelMinimize :: (GPattern ptrn, GAction actn) => Skeleton ptrn actn -> Skeleton ptrn actn
-- skelMinimize (Skeleton bones) = Skeleton bones''
--     where
--       bones' = selfMap f [] False bones
--       f ctxt isDisjoint bone1@(Bone ptrn act) bone2@(Bone ptrn' act')
--           -- Remove shadows
--           | ptrn' `P.match` ptrn = (isDisjoint, Just bone1, Nothing)
--           -- Remove fallthroughs
--           | ptrn `P.match` ptrn' && act == act' && not isDisjoint =
--               (False, Nothing, Just bone2) 
--           | otherwise = (isDisjoint || (act /= act' && conflict),
--                          Just bone1,
--                          Just bone2)
--         where 
--             ctxtmeets = mapMaybe (P.intersect ptrn) (Prelude.map (\(Bone ptrn actn) -> ptrn) ctxt)
--             conflict = overlap ptrn ptrn' && P.unsafeIntersect ptrn ptrn' `List.notElem` ctxtmeets
                              
--       -- remove bottom elements from end of classifier
--       bones'' = reverse $ dropWhile (\(Bone ptrn actn) -> actn == actnController) $ reverse bones'

--
-- compile
--
          
{-| Compile a policy to a classifier. -}
-- This currently doesn't implement the full consistent algorithm
{-   - [T | P | Rest] [T' | P' | Rest']
    throw away T'
    patInverse P, T
    intersect Rest' with P
    shadow removal P' | Rest' using T

    - Intersect P' | Rest' with P.
    - If we ever encounter something equal or greater, and it has the same action, stop and return yes.
    - Then do shadow removal. 

    We can also sometimes remove if iptrn == nothin -}
compile :: (ValidClassifier ptrn actn) => Switch -> Policy -> Classifier ptrn actn
compile s po = Classifier $ map f $ unpack skel
  where
    f (Bone sptrn iptrn (actn1, actn2)) 
      | toPattern sptrn `match` iptrn && actn1 == actn2 = (sptrn, actnTranslate actn1) 
      | otherwise = (sptrn, actnController)                                        
    skel = compilePolicy s po

{-| Return a supplemental classifier obtained from specializing the policy with the transmission. |-}
specialize :: (ValidEnvironment ptrn actn pkt) =>
              Transmission ptrn pkt -> Policy -> Classifier ptrn actn
specialize tr po = prune (trPkt tr) $ compile (trSwitch tr) (expandPolicy tr po)






skelActLift :: (a -> a -> a) -> ((a, a) -> (a, a) -> (a, a))
skelActLift f = \(a1, a2) (a1', a2') -> (f a1 a1', f a2 a2') 
  
                                        
compilePredicate :: forall ptrn. (GPattern ptrn) => Switch -> Predicate -> Skeleton ptrn Bool 
compilePredicate s (PrPattern pat) = Skeleton [Bone (fromPatternOverapprox pat) pat (True, True)]
compilePredicate s (PrSwitchPattern _ dyn) =
  case fromDynamic dyn :: Maybe ptrn of
    Just ptrn -> Skeleton [Bone ptrn undefined (True, True)]
    Nothing -> Skeleton []
--compilePredicate s (PrInspect ins) = Skeleton [Bone P.top True] -- this needs to be a maybe! FIX
compilePredicate s (PrTo s') | s == s' = Skeleton [Bone top top (True, True)]
                             | otherwise = Skeleton []
compilePredicate s (PrIntersect pr1 pr2) = skel12'
    where
      skel1 = compilePredicate s pr1
      skel2 = compilePredicate s pr2
      (skel12', skel1', skel2') = skelCart (skelActLift ( &&)) skel1 skel2
compilePredicate s (PrUnion pr1 pr2) = skel12' +++ skel1' +++ skel2'
    where
      skel1 = compilePredicate s pr1
      skel2 = compilePredicate s pr2
      (skel12', skel1', skel2') = skelCart (skelActLift ( ||)) skel1 skel2
compilePredicate s (PrNegate pr) = skelMap (\(a1, a2) -> (not a2, not a1)) (compilePredicate s pr) +++
                                   Skeleton [Bone top top (True, True)]


compilePolicy :: (GPattern ptrn) => Switch -> Policy -> Skeleton ptrn Actions
compilePolicy s (PoBasic po as) = 
    skelMap f $ compilePredicate s po
  where
    f (True, True) = (as,  as)
    f (False, True ) = (Set.empty, as)
    f (False, False) = (Set.empty,  Set.empty)
    
--compilePolicy s (PoDoer doer) = Skeleton [Bone P.top Set.empty] -- this needs to be a maybe! FIX!
compilePolicy s (PoUnion po1 po2) = skel12' +++ skel1' +++ skel2' 
    where
      skel1 = compilePolicy s po1
      skel2 = compilePolicy s po2
      (skel12', skel1', skel2') = skelCart (skelActLift Set.union) skel1 skel2
compilePolicy s (PoIntersect po1 po2) = skel12'
    where
      skel1 = compilePolicy s po1
      skel2 = compilePolicy s po2
      (skel12', skel1', skel2') = skelCart (skelActLift  Set.intersection) skel1 skel2


expandPredicate :: forall ptrn pkt. (ValidTransmission ptrn pkt) =>
                   Transmission ptrn pkt -> Predicate -> Predicate
expandPredicate tr (PrPattern ptrn) =
  case (fromPatternUnderapprox  (toPacket $ trPkt tr)  ptrn :: Maybe ptrn) of
      Just pat -> PrUnion (PrPattern ptrn) (PrSwitchPattern (show pat) (toDyn pat))
      Nothing -> PrPattern ptrn
--expandPredicate tr (PrInspect ins) = undefined
expandPredicate tr (PrSwitchPattern s dyn) = PrSwitchPattern s dyn
expandPredicate tr (PrTo s) = PrTo s
expandPredicate tr (PrUnion pr1 pr2) =
    PrUnion (expandPredicate tr pr1) (expandPredicate tr pr2)
expandPredicate tr (PrIntersect pr1 pr2) =
    PrIntersect (expandPredicate tr pr1) (expandPredicate tr pr2)
expandPredicate tr (PrDifference pr1 pr2) =
    PrDifference (expandPredicate tr pr1) (expandPredicate tr pr2)
expandPredicate tr (PrNegate pr) = PrNegate (expandPredicate tr pr)


expandPolicy :: (ValidTransmission ptrn pkt) =>
                Transmission ptrn pkt -> Policy -> Policy
expandPolicy tr (PoBasic pr as) = PoBasic (expandPredicate tr pr) as
--expandPolicy tr (PoDoer doer) = undefined
expandPolicy tr (PoUnion po1 po2) = PoUnion (expandPolicy tr po1) (expandPolicy tr po2)
expandPolicy tr (PoIntersect po1 po2) = PoIntersect (expandPolicy tr po1) (expandPolicy tr po2)
expandPolicy tr (PoDifference po1 po2) = PoDifference (expandPolicy tr po1) (expandPolicy tr po2)
