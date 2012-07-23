module Frenetic.NetCore.Reduce
  ( reduce
  ) where

import qualified Data.Set as Set
import Frenetic.NetCore

-- |Reduce the policy to produce a smaller, more readable policy
reduce = reducePo

reducePo :: Policy -> Policy
reducePo PoBottom = PoBottom
reducePo (PoBasic pr act) = PoBasic pr' act' where
  pr' = reducePr pr
  act' = act
-- Note that because we use multiset forwarding semantics, we CANNOT do common
-- subexpression reduction on unions.
reducePo (PoUnion p1 p2) = PoUnion p1' p2' where
  p1' = reducePo p1
  p2' = reducePo p2

-- TODO(astory): do reductions beyond common subexpression reduction.  Ideas:
-- top reduction, bottom reduction, empty intersection reduction.
reducePr :: Predicate -> Predicate
reducePr p@(PrUnion _ _) = prNaryUnion leaves where
  leaves = Set.toList . Set.fromList . map reducePr . prUnUnion $ p

reducePr p@(PrIntersect _ _) = prNaryIntersect leaves where
  leaves = Set.toList . Set.fromList . map reducePr . prUnIntersect $ p

reducePr (PrNegate (PrNegate p)) = reducePr p
reducePr (PrNegate p) = PrNegate (reducePr p)

reducePr p = p
