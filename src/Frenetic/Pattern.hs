
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
-- /src/Frenetic/Pattern.hs                                                   --
-- Patterns                                                                   --
-- $Id$ --
--------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables,
             ParallelListComp,
             GeneralizedNewtypeDeriving #-}

module Frenetic.Pattern where

import Data.List hiding (intersect)
import Data.Bits
import Data.Word
import Data.Maybe
import Data.HList
import Numeric (showHex)
    
{-|
A class for types that compose similar to wildcards.

All instances must satisfy the following:

* @match@ defines a partial order; @top@ is the top element of this order
  and @intersect@ is a meet.

* Meets are exact: if @match x y@ and @match x z@, then
  @match x (fromJust (intersect y z))@, if such a meet exists.

Minimal complete definition: top and intersect.
-}
class (Eq a) => Matchable a where
    top :: a
    intersect :: a -> a -> Maybe a
    match :: a -> a -> Bool
    overlap :: a -> a -> Bool
    disjoint :: a -> a -> Bool
    match x y = intersect x y == Just x
    overlap x y = isJust $ intersect x y
    disjoint x y = isNothing $ intersect x y

                   
data Wildcard a = Wildcard a a  -- Data and mask, respectively.

instance (Bits a) => Eq (Wildcard a) where
    (Wildcard x m) == (Wildcard x' m') =
        m == m' && x .|. m == x' .|. m

instance (Bits a, Integral a, Show a) => Show (Wildcard a) where
    show (Wildcard x m) | m == (complement 0) = "*"
                        | otherwise = if any (\c -> c == '?') s
                                      then s
                                      else ("0x" ++ showHex x "")
        where
          s = [f i | i <- reverse [0 .. n-1]] 
          n = bitSize x

          f i | testBit m i = '?'
              | testBit x i = '1'
              | otherwise = '0'

-- "Maybe" for exact matches. Don't be confused.
instance (Eq a) => Matchable (Maybe a) where
  top = Nothing
  intersect x Nothing = Just x
  intersect Nothing x = Just x
  intersect m1 m2 | m1 == m2 = Just m1
                  | otherwise = Nothing

intersectWildcard :: (Bits a) => Wildcard a -> Wildcard a -> Wildcard a
intersectWildcard (Wildcard x m) (Wildcard x' m') =
    Wildcard ((x .|. m) .&. (x' .|. m')) (m .&. m')

instance (Bits a) => Matchable (Wildcard a) where
    top = Wildcard 0 (complement 0)

    intersect a b = Just $ intersectWildcard a b

    overlap (Wildcard x m) (Wildcard x' m') = x .|. m'' == x' .|. m''
        where
          m'' = m .|. m'
          
    match (Wildcard x m) (Wildcard x' m') =
        m .&. m' == m  &&  x .|. m' == x' .|. m' 
         
    disjoint w w' = not $ overlap w w'
    
wMatch :: (Bits a) => a -> Wildcard a -> Bool
wMatch b w = wBitsMake b `match` w

wBitsMake :: (Bits a) => a -> Wildcard a
wBitsMake b = Wildcard b 0 
                    
wMake :: forall a. (Bits a) => String -> Wildcard a
wMake s = foldl' intersectWildcard top mds
    where
      n = bitSize (undefined :: a)

      mds = [f i c | c <- s | i <- reverse [0 .. n - 1]]

      f i '0' = Wildcard (complement $ bit i) (complement $ bit i)
      f i '1' = Wildcard (complement 0) (complement $ bit i)
      f i '?' = top
      f i _ = error "Bit other than {0, 1, ?}"

wReplaceData :: a -> Wildcard a -> Wildcard a
wReplaceData x (Wildcard x' m) = Wildcard x m
              
wReplaceMask :: a -> Wildcard a -> Wildcard a
wReplaceMask m (Wildcard x m') = Wildcard x m

wMake8 :: String -> Wildcard Word8
wMake8  = wMake

wMake16 :: String -> Wildcard Word16
wMake16 = wMake
              
wMake32 :: String -> Wildcard Word32
wMake32 = wMake

wMake64 :: String -> Wildcard Word64
wMake64 = wMake

showAbridged :: String -> String
showAbridged = reverse . ('*' :) . dropWhile (== '?') . reverse

-- Arbitrary "tuples"

instance Matchable HNil where
    top = HNil
    intersect _ _ = Just HNil

instance (Matchable a, Matchable b) => Matchable (HCons a b) where
    top = HCons top top
    intersect (HCons x y) (HCons x' y') = do x'' <- intersect x x'
                                             y'' <- intersect y y'
                                             Just $ HCons x'' y''

-- Restricted wildcards

{-|
This class allows one to approximate wildcards with restricted variants.

All instances must obey the laws!

* /Approximations/: For all @x@, we have @match x (overapprox x)@ and
  @match (underapprox x) x@.

* /Tightest/: There is no @y@ such that (1) @match x y@ and
   @match y (overapprox x)@ and (2) @match (underapprox x) y@ and
   @match y x@.

* /Monotonic/: If @match x y@, then (1) @match (overapprox x) (overapprox y)@
  and (2) if the data of x and y are the same, then
  @match (underapprox x) (underapprox y).
-}
class Approx a where
    overapprox :: (Bits b) => Wildcard b -> a b
    underapprox :: (Bits b) => Wildcard b -> b -> Maybe (a b)
    inverseapprox :: (Bits b) => a b -> Wildcard b

-- Prefix patterns

newtype Prefix a = Prefix (Wildcard a)
    deriving (Eq, Matchable)

instance (Bits a, Integral a, Show a) => Show (Prefix a) where
    show (Prefix w) = show w

instance Approx Prefix where
    overapprox w@(Wildcard x m) = Prefix (Wildcard x m')
        where
          n = bitSize m
          m' = case elemIndex True $ map (testBit m) $ reverse [0 .. n - 1] of
                 Just i -> foldl' setBit m $ reverse [0 .. (n - i - 2)]
                 Nothing -> m

    underapprox w@(Wildcard x m) x' | match (wBitsMake x') w = Just $ Prefix (Wildcard x' m')
                                    | otherwise = Nothing
        where
          n = bitSize m
          m' = case elemIndex False $ map (testBit m) [0 .. n - 1] of
                 Just i -> foldl' clearBit m [i + 1 .. n - 1]
                 Nothing -> m
 
    inverseapprox (Prefix w) = w
                            
-- Exact patterns

instance Approx Maybe where
    overapprox (Wildcard x m) | m == 0 = Just x
                              | otherwise = Nothing
    underapprox (Wildcard x m) x' | m == complement 0 && x == x' = Just (Just x)
                                  | otherwise = Nothing
    inverseapprox (Just x) = wBitsMake x
    inverseapprox Nothing = top



