
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
-- /src/Frenetic/Pattern.hs                                                   --
-- Patterns                                                                   --
--------------------------------------------------------------------------------

{-# LANGUAGE
             ParallelListComp,
             GeneralizedNewtypeDeriving #-}
module Frenetic.Pattern
  ( Matchable (..)
  , Wildcard (..)
  , Prefix (..)
  , exact
  , wild
  , wMatch
  ) where

import Data.List hiding (intersect)
import Data.Bits
import Data.Word
import Data.Maybe
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

-- |Wildcard that only matches this value
exact :: (Bits a) => a -> Wildcard a
exact value = Exact value

wild :: (Bits a) => Wildcard a
wild = Wildcard

data Wildcard a
  = Exact a
  | Wildcard
  deriving (Ord, Eq)

data Prefix a = Prefix a Int
  deriving (Ord, Eq)

instance Show a => Show (Wildcard a) where
  show Wildcard  = "*"
  show (Exact a) = show a

instance Functor Wildcard where
  fmap f (Exact a) = Exact (f a)
  fmap _ Wildcard  = Wildcard

instance (Bits a, Show a) => Show (Prefix a) where
  show (Prefix val significantBits) = case bitSize val == significantBits of
    True -> show val
    False -> "Prefix " ++ show val ++ " " ++ show significantBits

instance Bits a => Matchable (Prefix a) where
  top = Prefix 0 0
  intersect (Prefix v1 sig1) (Prefix v2 sig2) =
    let sig = min sig1 sig2 -- shorter prefix
        width = bitSize v1 -- value ignored
        mask = complement (bit (width - sig) - 1) in -- mask out lower bits
      case v1 .&. mask == v2 .&. mask of
        True -> case sig1 > sig2 of
                  True  -> Just (Prefix v1 sig1)
                  False -> Just (Prefix v2 sig2)
        False -> Nothing

instance Eq a => Matchable (Wildcard a) where
  top = Wildcard
  intersect (Exact a) (Exact b) = case a == b of
    True  -> Just (Exact a)
    False -> Nothing
  intersect (Exact a) Wildcard = Just (Exact a)
  intersect Wildcard (Exact b) = Just (Exact b)
  intersect Wildcard Wildcard  = Just Wildcard

wMatch :: Eq a => a -> Wildcard a -> Bool
wMatch b w = (Exact b) `match` w
