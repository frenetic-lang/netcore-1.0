module Frenetic.NetCore.Reduce
  ( reduce
  ) where

import Data.Maybe
import qualified Data.MultiSet as MS
import qualified Data.Set as Set
import Frenetic.NetCore

-- |Reduce the policy to produce a smaller, more readable policy
reduce = reducePo

reducePo :: Policy -> Policy
reducePo PoBottom = PoBottom
reducePo (PoBasic pr act) = if pr' == neg top || act == empty
                              then PoBottom
                              else PoBasic pr' act' where
  pr' = reducePr pr
  act' = act
-- Note that because we use multiset forwarding semantics, we CANNOT do common
-- subexpression reduction on unions.
reducePo (PoUnion p1 p2) = if p1' == PoBottom then p2'
                           else if p2' == PoBottom then p1'
                           else PoUnion p1' p2' where
  p1' = reducePo p1
  p2' = reducePo p2

-- TODO(astory): do reductions beyond common subexpression reduction.  Ideas:
-- top reduction, bottom reduction, empty intersection reduction.
reducePr :: Predicate -> Predicate
reducePr p@(PrUnion _ _) = prOr leaves where
  leaves = Set.toList . Set.fromList . map reducePr . prUnUnion $ p

reducePr p@(PrIntersect _ _) = result where
  leaves = Set.toList . Set.fromList . map reducePr . prUnIntersect $ p
  nSwitches = Set.size .Set.fromList . catMaybes . map switchOfPred $ leaves
  result = if nSwitches > 1 then neg top
           else prAnd leaves

reducePr (PrNegate (PrNegate p)) = reducePr p
reducePr (PrNegate p) = PrNegate (reducePr p)

reducePr p = p

switchOfPred (PrTo s) = Just s
switchOfPred _ = Nothing
