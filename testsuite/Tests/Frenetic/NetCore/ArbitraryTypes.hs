{-# LANGUAGE
    TypeSynonymInstances
 #-}

module Tests.Frenetic.NetCore.ArbitraryTypes where

import Data.Set                                 as Set
import Control.Monad
import Frenetic.NetCore.Types
import Tests.Frenetic.ArbitraryCompat
import Tests.Frenetic.Switches.ArbitraryOpenFlow
import Test.QuickCheck

instance Arbitrary Policy where
  -- Bound the size of the policy
  arbitrary = sized $ \s -> let depth = 10 in
    if s > depth
      then resize depth arbitrary
      else if s > 0
        then do
          pred <- resize s arbitrary
          acts <- resize s arbitrary
          --p1   <- resize (s-1) arbitrary
          --p2   <- resize (s-1) arbitrary
          oneof [ return $ PoBasic pred acts
                --, return $ PoUnion p1 p2
                  -- TODO(arjun): test putting union into here and un-comment
                  -- code
                ]
        else do
          pred <- resize s arbitrary
          acts <- resize s arbitrary
          return $ PoBasic pred acts
  shrink (PoBottom)             = [PoBottom]
  shrink (PoBasic pr as)        = [PoBasic pr' as | pr' <- shrink pr]
  shrink (PoUnion p1 p2)        = [p1, p2]
