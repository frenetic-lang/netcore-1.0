module Frenetic.NetCore.Reduce
  ( reduce
  ) where

import Frenetic.Common
import Data.List (nub)
import Data.Maybe
import qualified Data.Set as Set
import Frenetic.NetCore.Types
import Frenetic.NetCore.Short
import Frenetic.Pattern
import Debug.Trace

-- |Reduce the policy to produce a smaller, more readable policy
reduce = reducePo

reducePo :: Policy -> Policy
reducePo PoBottom = PoBottom
reducePo (PoBasic pr act) = if pr' == None || act == mempty
                              then PoBottom
                              else PoBasic pr' act' where
  pr' = pr
  act' = act
-- Note that because we use multiset forwarding semantics, we CANNOT do common
-- subexpression reduction on unions.
reducePo (PoUnion p1 p2) = 
  let p1' = reducePo p1
      p2' = reducePo p2
    in case (p1', p2') of
         (PoBottom, _) -> p2'
         (_, PoBottom) -> p1'
         otherwise ->  PoUnion p1' p2' where
reducePo (Restrict pol pred) = Restrict (reducePo pol) pred
reducePo (SendPackets chan) = SendPackets chan

isNonNegatedAtom pred = case pred of
  DlSrc _ -> True
  DlDst _ -> True
  DlTyp _ -> True
  DlVlan _ -> True
  DlVlanPcp _ -> True
  NwSrc _ -> True
  NwDst _ -> True
  NwProto _ -> True
  NwTos _ -> True
  TpSrcPort _ -> True
  TpDstPort _ -> True
  IngressPort _ -> True
  Switch _ -> True
  Or _ _ -> False
  And _ _ -> False
  Not _ -> True
  Any -> True
  None -> True

isAtom (Not x) = isNonNegatedAtom x
isAtom x = isNonNegatedAtom x

isConjunction (Or _ _) = False
isConjunction (And pr1 pr2) = isConjunction pr1 && isConjunction pr2
isConjunction x = isAtom x

dnf :: Predicate -> Predicate
dnf pr = case pr of
  Not (Not pr') -> dnf pr'
  Not (Or pr1 pr2) -> dnf (And (Not pr1) (Not pr2))
  Not (And pr1 pr2) -> dnf (Or (Not pr1) (Not pr2))
  Or pr1 pr2 -> Or (dnf pr1) (dnf pr2)
  And x (Or y z) -> 
    Or (dnf (And x y)) (dnf (And x z))
  And (Or x y) z ->
     Or (dnf (And x z)) (dnf (And y z))
  And x y ->
    let x' = dnf x
        y' = dnf y in
    if isConjunction x' && isConjunction y' then
      And x' y'
    else
      dnf (And x' y')
  otherwise ->
    if isAtom pr then pr else error ("missing case in dnf " ++ show pr)

disjList (Or pr1 pr2) = Or (disjList pr1) (disjList pr2)
disjList x = case simplify (conjList x) of
  [] -> None
  x:xs -> foldl And x xs

conjList (And pr1 pr2) = conjList pr1 ++ conjList pr2
conjList x = [x]

isAny Any = True
isAny _ = False

isNone None = True
isNone _ = False

-- Simplifies a conjunction
simplify :: [Predicate] -> [Predicate]
simplify atomList = Set.toList result
  where result = if None `Set.member` atoms then Set.empty else atoms
        atoms = Set.fromList (filter isAny atomList)
