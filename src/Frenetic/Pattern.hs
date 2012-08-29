module Frenetic.Pattern
  ( Matchable (..)
  ) where

import Data.Maybe

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

