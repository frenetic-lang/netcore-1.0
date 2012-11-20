module Frenetic.NetCore.Reduce
  ( reduce
  , isEmptyPredicate
  , dnf
  , flattenDNF
  , flattenConj
  , isEmptyConj
  ) where

import Frenetic.Common
import Data.List (nub, partition)
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as M
import qualified Frenetic.CoFiniteSet as CFS
import Frenetic.NetCore.Types
import Frenetic.NetCore.Short
import Frenetic.NetCore.Util (Field (..))
import Frenetic.Pattern
import Debug.Trace
import Nettle.Ethernet.EthernetAddress (unpackEth64)
import Nettle.IPv4.IPAddress (IPAddressPrefix (..), IPAddress (..))

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
-- mjr: I'm sure there's something we can do here
reducePo (Sequence pol1 pol2) = Sequence (reducePo pol1) (reducePo pol2)
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

isAtom (Not Any) = False
isAtom (Not None) = False
isAtom (Not x) = isNonNegatedAtom x
isAtom x = isNonNegatedAtom x

isConjunction (Or _ _) = False
isConjunction (And pr1 pr2) = isConjunction pr1 && isConjunction pr2
isConjunction x = isAtom x

isConjOpt (Or _ _) = False
isConjOpt _ = True

flattenDNF :: Predicate -> [[Predicate]]
flattenDNF (Or pr1 pr2) = flattenDNF pr1 ++ flattenDNF pr2
flattenDNF conj = case flattenConj conj of
  Just atoms -> if isEmptyConj atoms then [] else [atoms]
  Nothing -> []

flattenConj :: Predicate -> Maybe [Predicate]
flattenConj (And pr1 pr2) = do
  atoms1 <- flattenConj pr1
  atoms2 <- flattenConj pr2
  return (atoms1 ++ atoms2)
flattenConj None = Nothing
flattenConj Any = Just []
flattenConj atom = Just [atom]

atomKV :: Predicate -> (Field, CFS.CoFiniteSet Integer)
atomKV (DlSrc x) = (FDlSrc, CFS.singleton (fromIntegral (unpackEth64 x)))
atomKV (DlDst x) = (FDlDst, CFS.singleton (fromIntegral (unpackEth64 x)))
atomKV (DlTyp x) = (FDlTyp, CFS.singleton (fromIntegral x))
atomKV (DlVlan Nothing) = (FDlVlan, CFS.singleton 0xffff)
atomKV (DlVlan (Just x)) = (FDlVlan, CFS.singleton (fromIntegral x))
atomKV (DlVlanPcp x) = (FDlVlanPcp, CFS.singleton (fromIntegral x))
atomKV (NwSrc (IPAddressPrefix (IPAddress x) 32)) = 
  (FNwSrc, CFS.singleton (fromIntegral x))
atomKV (NwDst (IPAddressPrefix (IPAddress x) 32)) = 
  (FNwDst, CFS.singleton (fromIntegral x))
atomKV (NwProto x) = (FNwProto, CFS.singleton (fromIntegral x))
atomKV (NwTos x) = (FNwTos, CFS.singleton (fromIntegral x))
atomKV (TpSrcPort x) = (FTpSrc, CFS.singleton (fromIntegral x))
atomKV (TpDstPort x) = (FTpDst, CFS.singleton (fromIntegral x))
atomKV (IngressPort x) = (FInPort, CFS.singleton (fromIntegral x))
atomKV (Switch x) = (FSwitch, CFS.singleton (fromIntegral x))
atomKV (Not (DlSrc x)) = (FDlSrc, CFS.excludes (fromIntegral (unpackEth64 x)))
atomKV (Not (DlDst x)) = (FDlDst, CFS.excludes (fromIntegral (unpackEth64 x)))
atomKV (Not (DlTyp x)) = (FDlTyp, CFS.excludes (fromIntegral x))
atomKV (Not (DlVlan Nothing)) = (FDlVlan, CFS.excludes 0xffff)
atomKV (Not (DlVlan (Just x))) = (FDlVlan, CFS.excludes (fromIntegral x))
atomKV (Not (DlVlanPcp x)) = (FDlVlanPcp, CFS.excludes (fromIntegral x))
atomKV (Not (NwSrc (IPAddressPrefix (IPAddress x) 32))) = 
  (FNwSrc, CFS.excludes (fromIntegral x))
atomKV (Not (NwDst (IPAddressPrefix (IPAddress x) 32))) = 
  (FNwDst, CFS.excludes (fromIntegral x))
atomKV (Not (NwProto x)) = (FNwProto, CFS.excludes (fromIntegral x))
atomKV (Not (NwTos x)) = (FNwTos, CFS.excludes (fromIntegral x))
atomKV (Not (TpSrcPort x)) = (FTpSrc, CFS.excludes (fromIntegral x))
atomKV (Not (TpDstPort x)) = (FTpDst, CFS.excludes (fromIntegral x))
atomKV (Not (IngressPort x)) = (FInPort, CFS.excludes (fromIntegral x))
atomKV (Not (Switch x)) = (FSwitch, CFS.excludes (fromIntegral x))
atomKV _ = error "atomKV: not an atom"

isEmptyConj :: [Predicate] -> Bool
isEmptyConj atoms = loop M.empty atoms where  
  loop :: Map Field (CFS.CoFiniteSet Integer) -> [Predicate] -> Bool
  loop _ [] = False
  loop sets (atom : rest) =
    let (k, v) = atomKV atom
      in case M.lookup k sets of
           Nothing -> loop (M.insert k v sets) rest
           Just set -> case CFS.null (CFS.inter set v) of
             True -> True
             False -> loop (M.insert k (CFS.inter set v) sets) rest

isEmptyPredicate :: Predicate -> Bool
isEmptyPredicate pred = null (flattenDNF (dnf pred))

dnf :: Predicate -> Predicate
dnf pr = case pr of
  Not Any -> None
  Not None -> Any
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
