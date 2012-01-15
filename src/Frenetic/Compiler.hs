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

import System.IO.Unsafe
import System.IO
import Data.Bits
import Data.Map as Map
import Data.Set as Set
import Data.Word
import Data.List as List

import Frenetic.Network 
import Frenetic.Patterns
import Frenetic.Language
import Frenetic.Switches.OpenFlow
    
import Nettle.OpenFlow.Match as OFMatch
import Nettle.OpenFlow.Action as OFAction
import Nettle.IPv4.IPAddress as IPAddress 
import Nettle.Ethernet.EthernetAddress 

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


data Bone a = Bone OFMatch.Match a
data Skeleton a = Skeleton [Bone a]

instance (Show a) => Show (Bone a) where
  show (Bone m a) = "Bone(" ++ show m ++ "," ++ show a ++ ")"

instance (Show a) => Show (Skeleton a) where
  show (Skeleton bs) = concat (intersperse "\n" $ Prelude.map show bs)

(+++) :: Skeleton a -> Skeleton a -> Skeleton a
(+++) (Skeleton bs1) (Skeleton bs2) = Skeleton (bs1 ++ bs2)

skelMap :: (a -> b) -> Skeleton a -> Skeleton b 
skelMap f (Skeleton bs) = Skeleton $ Prelude.map (\(Bone m a) -> Bone m (f a )) bs

skelCart :: (a -> a -> a) -> Skeleton a -> Skeleton a -> (Skeleton a, Skeleton a, Skeleton a)
skelCart f (Skeleton bs1) (Skeleton bs2) = 
  let h bs x@(Bone m1 a1) y@(Bone m2 a2) = 
        case mMeet m1 m2 of 
          Just m12 -> 
            (bs ++ [Bone m12 (f a1 a2)],
             if m12 == m1 then Nothing else Just x,
             if m12 == m2 then Nothing else Just y)
          Nothing -> 
            (bs, Just x, Just y) in 
  let (bs1',bs2',bs3') = cartMap h [] bs1 bs2 in 
  (Skeleton bs1', Skeleton bs2', Skeleton bs3')

instance (Lattice a) => Lattice (Skeleton a) where
  top = Skeleton [Bone mTop top]
  bottom = Skeleton []
  join skel1 skel2 = skel12 +++ skel1' +++ skel2' 
    where (skel12, skel1', skel2') = skelCart join skel1 skel2
  meet skel1 skel2 = skel12
    where (skel12,_,_) = skelCart meet skel1 skel2

instance (BooleanAlgebra a) => BooleanAlgebra (Skeleton a) where
  neg skel1 = skelMap neg skel1 +++ top 

compileAction :: Frenetic.Language.Action -> OFAction.Action
compileAction (AForward n) = 
  SendOutPort (PhysicalPort n)

compileActions :: Frenetic.Language.Actions -> [OFAction.Action]
compileActions (PSet as) = Set.fold (\a l -> compileAction a : l) [] as
compileActions (NSet as) = error "Cannot handle cofinite actions"

compilePredicate :: Switch -> Predicate p -> Skeleton Bool 
compilePredicate s (EInport n) = 
  Skeleton [Bone (inportExactMatch n) True,
            Bone mTop False] 
compilePredicate s (EHeader h mb) =   
  Skeleton [Bone (headerExactMatch h mb) True,
            Bone mTop False] 
compilePredicate s (ESwitch s') | s == s' = Skeleton [Bone mTop True]
                                | otherwise = Skeleton [Bone mTop False]
compilePredicate s (EIntersect pr1 pr2) = 
  compilePredicate s pr1 /\ compilePredicate s pr2 
compilePredicate s (EUnion pr1 pr2) = 
  compilePredicate s pr1 \/ compilePredicate s pr2 
compilePredicate s (ENegate pr1) = 
  neg $ compilePredicate s pr1

compilePolicy :: Switch -> Policy p -> Skeleton Frenetic.Language.Actions
compilePolicy s (PBasic pr1 as) = 
  skelMap (\b -> if b then as else PSet (Set.empty)) $ compilePredicate s pr1
compilePolicy s (PUnion p1 p2) = 
  compilePolicy s p1 \/ compilePolicy s p2 
compilePolicy s (PIntersect p1 p2) = 
  compilePolicy s p1 /\ compilePolicy s p2
  
compile :: Switch -> Policy p -> [Rule] 
compile s p = 
  Prelude.map (\(Bone match actions) -> Rule match (compileActions actions)) bones
  where Skeleton bones = compilePolicy s p  

specialize :: Policy p -> Switch -> Transmission p -> [Rule]
specialize policy switch t@(Transmission _ port pkt) = []
  -- let actions = Prelude.map compileAction $ (Set.toList (interpretPolicy policy t)) in 
  -- let match = 
  --      Match { OFMatch.inPort = Just port,
  --              OFMatch.srcEthAddress = Just $ (\ (HardwareAddress e) -> e) $ getHeader pkt Dl_src,
  --              OFMatch.dstEthAddress = Just $ (\ (HardwareAddress e) -> e) $ getHeader pkt Dl_dst,
  --              OFMatch.vLANID = Just $ getHeader pkt Dl_vlan, 
  --              OFMatch.vLANPriority = Just $ getHeader pkt Dl_vlan_pcp, 
  --              OFMatch.ethFrameType = Just $ getHeader pkt Dl_typ,
  --              OFMatch.ipTypeOfService = Just $ getHeader pkt Nw_tos,
  --              OFMatch.ipProtocol = Just $ getHeader pkt Nw_proto,
  --              OFMatch.srcIPAddress = ((IPAddress $ getHeader pkt Nw_src), 32),
  --              OFMatch.dstIPAddress = ((IPAddress $ getHeader pkt Nw_dst), 32),
  --              OFMatch.srcTransportPort = Just $ getHeader pkt Tp_src,
  --              OFMatch.dstTransportPort = Just $ getHeader pkt Tp_dst } in 
  -- [Rule match actions]
