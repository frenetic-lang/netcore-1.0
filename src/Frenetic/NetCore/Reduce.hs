module Frenetic.NetCore.Reduce
  ( reduce
  ) where

import Frenetic.Common
import Data.List (nub)
import Data.Maybe
import qualified Data.MultiSet as MS
import qualified Data.Set as Set
import Frenetic.NetCore.Types
import Frenetic.NetCore.Short
import Frenetic.Pattern
import Debug.Trace

-- |Reduce the policy to produce a smaller, more readable policy
reduce = reducePo

reducePo :: Policy -> Policy
reducePo PoBottom = PoBottom
reducePo (PoBasic pr act) = if pr' == matchNone || act == mempty
                              then PoBottom
                              else PoBasic pr' act' where
  pr' = simplToDnf pr
  act' = act
-- Note that because we use multiset forwarding semantics, we CANNOT do common
-- subexpression reduction on unions.
reducePo (PoUnion p1 p2) = if p1' == PoBottom then p2'
                           else if p2' == PoBottom then p1'
                           else PoUnion p1' p2' where
  p1' = reducePo p1
  p2' = reducePo p2

simplToDnf = disjList . dnf . explode

isPat (PrPattern _) = True
isPat _ = False

isSwitch (PrTo _) = True
isSwitch _ = False

getPat (PrPattern pat) = pat
getPat (PrNegate (PrPattern pat)) = pat
getPat _ = error "not a pat"

isPatComplement (PrNegate (PrPattern _)) = True
isPatComplement _ = False

elim :: [Predicate] -> [Predicate]
elim atoms = 
  let set = Set.fromList atoms
      (switches, notSwitches) = Set.partition isSwitch set
      (pats, notPats) = Set.partition isPat notSwitches
      rawPats = map getPat (Set.toList pats)
      clausePat = foldl f (Just top) rawPats where
          f Nothing _ = Nothing
          f (Just p1) p2 = intersect p1 p2
      patComplements = 
        map getPat (Set.toList (Set.filter isPatComplement notPats))
    in case Set.size switches > 1 of
         True -> []
         False -> case clausePat of
           Nothing -> []
           Just pat -> case any (overlap pat) patComplements of
             True -> []
             False -> Set.toList set
        
-- List of lists of atoms
disjList (PrUnion pr1 pr2) = PrUnion (disjList pr1) (disjList pr2)
disjList x = case elim (conjList x) of
  [] -> PrNegate (PrPattern top)
  x:xs -> foldl PrIntersect x xs

conjList (PrIntersect pr1 pr2) = conjList pr1 ++ conjList pr2
conjList x = [x]

explode (PrPattern pat) = case explodePattern pat of
  [] -> top
  (x:xs) -> foldr PrIntersect x xs
explode (PrNegate p) = PrNegate (explode p)
explode (PrUnion p1 p2) = PrUnion (explode p1) (explode p2)
explode (PrIntersect p1 p2) = PrIntersect (explode p1) (explode p2)
explode (PrTo sw) = PrTo sw

isConjunction (PrPattern _) = True
isConjunction (PrTo _) = True
isConjunction (PrUnion _ _) = False
isConjunction (PrIntersect pr1 pr2) = isConjunction pr1 && isConjunction pr2
isConjunction x = isAtom x

isAtom (PrTo _) = True
isAtom (PrPattern _) = True
isAtom (PrNegate (PrTo _)) = True
isAtom (PrNegate (PrPattern _)) = True
isAtom _ = False

dnf :: Predicate -> Predicate
dnf pr = case pr of
  PrPattern _ -> pr
  PrTo _ -> pr
  PrNegate (PrTo _) -> pr
  PrNegate (PrPattern _) -> pr
  PrNegate (PrNegate pr') -> dnf pr'
  PrNegate (PrUnion pr1 pr2) -> dnf (PrIntersect (PrNegate pr1) (PrNegate pr2))
  PrNegate (PrIntersect pr1 pr2) -> dnf (PrUnion (PrNegate pr1) (PrNegate pr2))
  PrUnion pr1 pr2 -> PrUnion (dnf pr1) (dnf pr2)
  PrIntersect x (PrUnion y z) -> 
    PrUnion (dnf (PrIntersect x y)) (dnf (PrIntersect x z))
  PrIntersect (PrUnion x y) z ->
     PrUnion (dnf (PrIntersect x z)) (dnf (PrIntersect y z))
  PrIntersect x y ->
    let x' = dnf x
        y' = dnf y in
    if isConjunction x' && isConjunction y' then
      PrIntersect x' y'
    else
      dnf (PrIntersect x' y')
