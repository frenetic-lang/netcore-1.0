-- |Functions to transforma and inspect policies that are useful for certain
-- advanced operations.
module Frenetic.NetCore.Util
  ( transActions
  , policyImage
  ) where

import Frenetic.Common
import Frenetic.NetCore.Types
import qualified Data.MultiSet as MS

transActions :: (Action -> Action) 
             -> Policy 
             -> Policy
transActions f pol = case pol of
  PoBottom -> PoBottom
  PoBasic pred acts -> PoBasic pred (MS.map f acts)
  PoUnion pol1 pol2 -> PoUnion (transActions f pol1) (transActions f pol2)

policyImage :: Policy
            -> MS.MultiSet Action
policyImage pol = case pol of
  PoBottom -> MS.empty
  PoBasic _ acts -> acts
  PoUnion pol1 pol2 -> policyImage pol1 `MS.union` policyImage pol2