module Frenetic.CoFiniteSet
  ( CoFiniteSet
  , singleton
  , excludes
  , inter
  , null
  ) where

import Prelude hiding (null)
import qualified Data.Set as S

data CoFiniteSet a = All | Finite (S.Set a) | CoFinite (S.Set a)
  deriving (Show)

singleton :: Ord a => a -> CoFiniteSet a
singleton a = Finite (S.singleton a)

excludes :: Ord a => a -> CoFiniteSet a
excludes a = CoFinite (S.singleton a)

null :: Ord a => CoFiniteSet a -> Bool
null All = False
null (Finite elts) = S.null elts
null (CoFinite _) = False

inter :: Ord a => CoFiniteSet a -> CoFiniteSet a -> CoFiniteSet a
inter All x = x
inter x All = x
inter (Finite elts) (CoFinite excl) = Finite (elts `S.difference` excl)
inter (CoFinite excl) (Finite elts) = Finite (elts `S.difference` excl)
inter (Finite elts1) (Finite elts2) = Finite (elts1 `S.intersection` elts2)
inter (CoFinite excl1) (CoFinite excl2) = CoFinite (excl1 `S.union` excl2)
